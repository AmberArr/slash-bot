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
import Control.Monad.Trans.Maybe
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BC
import Data.Function
import Data.Has
import Data.Maybe
import qualified Data.Set as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Servant.Client
import System.Environment
import qualified Telegram.Bot.API as Tg
import qualified Telegram.Bot.Simple.UpdateParser as P

import Bot
import Config
import Db (DBConn, mkTable, withDBConn)
import Interface

data Action =
    Ping
  | BlackListAdd ![Text]
  | BlackListDel ![Text]
  | BlackListList
  | Reply !Text
  deriving (Show, Eq)

mkEnv :: BotConfig -> ClientEnv -> DBConn -> Env
mkEnv x y conn = (x, y, conn)

handleUpdate :: (WithBlacklist m, WithBot r m)
             => Tg.Update -> m ()
handleUpdate update = void $ runMaybeT $ do
  botUsername <- botCfgUsername <$> asks getter
  chatId <- MaybeT $ pure $ Tg.updateChatId update
  action <- MaybeT $ handle1 update botUsername <$> getBlacklist chatId
  message <- MaybeT $ pure $ update & Tg.updateMessage
  let messageId = Tg.messageMessageId message
  lift $ handleEffectful chatId messageId action

handle1 :: Tg.Update -> Text -> Blacklist -> Maybe Action
handle1 update botUsername blacklist = flip P.parseUpdate update $ do
  text <- P.text
  let words = T.words $ T.tail $ escape $ stripBotName text
  case T.head text of
    '/' -> activeVoiceHandler words
    '\\' -> passiveVoiceHandler words
    _ -> fail "not a command"

  where
    activeVoiceHandler words = case words of
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

    passiveVoiceHandler words = case words of
      [verb]                             -> pure $ Reply [i|#{sender} 被 #{recipient} #{verb} 了！|]
      (verb : (T.unwords -> patient))    -> pure $ Reply [i|#{sender} 被 #{recipient} #{verb}#{patient}！|]
      []                                 -> fail ""

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

handleEffectful :: (WithBlacklist m, WithBot r m)
                => Tg.ChatId -> Tg.MessageId -> Action -> m ()
handleEffectful chatId messageId action = case action of
  Ping -> reply' "111"
  BlackListAdd xs -> do
    forM_ xs $ addBlacklistItem chatId
    reply' "ok"
  BlackListDel xs -> do
    forM_ xs $ delBlacklistItem chatId
    reply' "ok"
  BlackListList -> do
    blacklist <- getBlacklist chatId
    let size = Set.size blacklist
        s = if size == 1 then "" else "s" :: Text
    reply' $ T.unlines
      [ [i|Blacklisted #{size} command#{s}:|]
      , T.intercalate " " (Set.toList blacklist)
      ]
  Reply x -> reply' x
  where
    reply' = sendTextTo (Tg.SomeChatId chatId) (Just messageId) . prependLTRMark 
    prependLTRMark x = "\8206" <> x

main :: IO ()
main = do
  token <- Tg.Token . T.pack <$> getEnv "TOKEN"
  clientEnv <- Tg.defaultTelegramClientEnv token
  Just botUsername <- Tg.userUsername <$>
    runReaderT (runTgApi_ Tg.getMe) clientEnv

  dburl <- BC.pack <$> getEnv "DATABASE_URL"
  withDBConn dburl $ \conn -> do
    mkTable conn
    startWebhook token $ \update -> runBotM
      (handleUpdate update)
      (mkEnv (BotConfig (Just Tg.HTML) botUsername) clientEnv conn)


startWebhook :: Tg.Token -> (Tg.Update -> IO ()) -> IO ()
startWebhook (Tg.Token token) handler = do
  Warp.runEnv 3000 $ \req respond -> do
    if pathInfo req == ["bot" <> token]
      then do
        mupdate <- decode <$> strictRequestBody req
        forM_ mupdate $ \update ->
          forkIO $ handler update
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
