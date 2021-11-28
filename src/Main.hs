{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.Aeson (decode)
import Data.Function
import Data.Has
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.List (foldl')
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (runEnv)
import Servant.Client
import System.Environment
import qualified Telegram.Bot.API as Tg
import qualified Telegram.Bot.Simple.UpdateParser as P

import Bot
import Config
import Interface.Bot

type Blacklist = Set Text
type Blacklists = HashMap Tg.ChatId Blacklist

data Action =
    Ping
  | BlackListAdd ![Text]
  | BlackListDel ![Text]
  | BlackListList
  | Reply !Text
  deriving (Show, Eq)

handleUpdate :: (MonadIO m, WithBot r m, MonadReader r m, Has ClientEnv r, Monad m)
             => Tg.Update -> ClientEnv -> IORef Blacklists -> m ()
handleUpdate update env blacklistsRef = do
  blacklists <- liftIO $ readIORef blacklistsRef
  botUsername <- fromJust . Tg.userUsername <$> getMe
  forM_ (Tg.updateChatId update) $ \chatId -> do
    let blacklist = fromMaybe Set.empty (HashMap.lookup chatId blacklists)
        maction = handle1 update botUsername blacklist
    case maction of
      Just action -> do
        newBlacklist <- handleEffectful update action blacklist
        liftIO $ atomicModifyIORef' blacklistsRef $
          \blacklists -> (HashMap.insert chatId newBlacklist blacklists, ())
      Nothing -> pure ()

handle1 :: Tg.Update -> Text -> Blacklist -> Maybe Action
handle1 update botUsername blacklist = flip P.parseUpdate update $ do
  text <- P.text
  when (T.head text /= '/') $ fail "not a command"

  case T.words $ T.tail $ escape $ stripBotName text of
    ("_ping" : _)                      -> pure Ping
    ("_blacklist" : "add" : xs)        -> pure $ BlackListAdd xs
    ("_blacklist" : "del" : xs)        -> pure $ BlackListDel xs
    ["_blacklist"]                     -> pure BlackListList
    ((T.break (== '@') -> (cmd, botName)) : _)
      | cmd `Set.member` blacklist || not (T.null botName)
                                       -> fail ""
    ["me"]                             -> fail ""
    ["you"]                            -> fail ""
    ("me" : (T.unwords -> predicate))  -> pure $ Reply [i|#{sender} #{predicate}！|]
    ("you" : (T.unwords -> predicate)) -> pure $ Reply [i|#{recipient} #{predicate}！|]
    [verb]                             -> pure $ Reply [i|#{sender} #{verb} 了 #{recipient}！|]
    (verb : (T.unwords -> patient))    -> pure $ Reply [i|#{sender} #{verb} #{recipient} #{patient}！|]
    []                                 -> fail ""
  where
    (sender0, senderId) = fromJust $
      update & (Tg.updateMessage
            >=> Tg.messageFrom
            >=> pure . (Tg.userFirstName &&& Tg.userId))
    sender = mention senderId sender0

    (recipient0, recipientId) = fromMaybe ("自己", senderId) $
      update & (Tg.updateMessage
            >=> Tg.messageReplyToMessage
            >=> Tg.messageFrom
            >=> pure . (Tg.userFirstName &&& Tg.userId))
    recipient = mention recipientId recipient0

    stripBotName text = case T.words text of
      ((T.stripSuffix ("@" <> botUsername) -> Just x) : xs) -> T.unwords (x:xs)
      _ -> text

handleEffectful :: (MonadIO m, WithBot r m, MonadReader r m, Has ClientEnv r, Monad m)
                => Tg.Update -> Action -> Blacklist -> m Blacklist
handleEffectful update actionInner blacklist = case actionInner of
  Ping -> blacklist <$ reply' "111"
  BlackListAdd xs ->
    foldl' (flip Set.delete) blacklist xs <$ reply' "ok"
  BlackListDel xs ->
    foldl' (flip Set.delete) blacklist xs <$ reply' "ok"
  BlackListList -> blacklist <$ do
    let size = Set.size blacklist
        s = if size == 1 then "" else "s" :: Text
    reply' $ T.unlines
      [ [i|Blacklisted #{size} command#{s}:|]
      , T.intercalate " " (Set.toList blacklist)
      ]
  Reply x -> blacklist <$ do
    reply' x
  where reply' = reply update

main :: IO ()
main = (Tg.Token . T.pack <$> getEnv "TOKEN") >>= startWebhook

startWebhook :: Tg.Token -> IO ()
startWebhook (Tg.Token token) = do
  env <- Tg.defaultTelegramClientEnv (Tg.Token token)
  blacklistsRef <- newIORef HashMap.empty
  runEnv 3000 $ \req respond -> do
    if pathInfo req == ["bot" <> token]
      then do
        mupdate <- decode <$> strictRequestBody req
        forM_ mupdate $ \update ->
          forkIO $ flip runBotM (mkEnv (BotConfig (Just Tg.HTML)) env) $ handleUpdate update env blacklistsRef
        respond $ responseLBS status200 [] ""
      else
        respond $ responseLBS status404 [] ""

mention :: Tg.UserId -> Text -> Text
mention (Tg.UserId userId) content =
  "<a href=\"tg://user?id=" <> T.pack (show userId) <> "\">" <> content <> "</a>"

escape :: Text -> Text
escape =
    T.replace "&" "&amp;"
  . T.replace "<" "&lt;"
  . T.replace ">" "&gt;"
