{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Monad
import Data.Aeson (decode)
import Data.Function
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
import Telegram.Bot.API as Tg
import qualified Telegram.Bot.Simple.UpdateParser as P

type Blacklist = Set Text
type Blacklists = HashMap ChatId Blacklist

data Action =
    Ping
  | BlackListAdd ![Text]
  | BlackListDel ![Text]
  | BlackListList
  | Reply !Text
  deriving (Show, Eq)

runTgApi :: ClientM (Tg.Response a) -> ClientEnv -> IO a
runTgApi client env = either (error . show) id <$>
  runClientM (fmap responseResult client) env

handleUpdate :: Update -> ClientEnv -> IORef Blacklists -> IO ()
handleUpdate update clientenv blacklistsRef = do
  blacklists <- readIORef blacklistsRef
  Just botUsername <- userUsername <$> runTgApi getMe clientenv
  let Just chatId = updateChatId update
  let blacklist = fromMaybe Set.empty (HashMap.lookup chatId blacklists)

  let maction = handle1 update botUsername blacklist
  case maction of
    Just action -> do
      newBlacklist <- handleEffectful action blacklist
      atomicModifyIORef' blacklistsRef $
        \blacklists -> (HashMap.insert chatId newBlacklist blacklists, ())
    Nothing -> pure ()

handle1 :: Update -> Text -> Blacklist -> Maybe Action
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
      update & (updateMessage
            >=> messageFrom
            >=> pure . (userFirstName &&& userId))
    sender = mention senderId sender0

    (recipient0, recipientId) = fromMaybe ("自己", senderId) $
      update & (updateMessage
            >=> messageReplyToMessage
            >=> messageFrom
            >=> pure . (userFirstName &&& userId))
    recipient = mention recipientId recipient0

    stripBotName text = case T.words text of
      ((T.stripSuffix ("@" <> botUsername) -> Just x) : xs) -> T.unwords (x:xs)
      _ -> text

handleEffectful :: Action -> Blacklist -> IO Blacklist
handleEffectful actionInner blacklist = case actionInner of
  Ping -> blacklist <$ reply "111"
  BlackListAdd xs ->
    foldl' (flip Set.delete) blacklist xs <$ reply "ok"
  BlackListDel xs ->
    foldl' (flip Set.delete) blacklist xs <$ reply "ok"
  BlackListList -> blacklist <$ do
    let size = Set.size blacklist
        s = if size == 1 then "" else "s" :: Text
    reply $ T.unlines
      [ [i|Blacklisted #{size} command#{s}:|]
      , T.intercalate " " (Set.toList blacklist)
      ]
  Reply x -> blacklist <$ do
    reply x

reply :: Text -> IO ()
reply x = void $ flip runTgApi undefined $ sendMessage SendMessageRequest
  { sendMessageChatId = undefined
  , sendMessageText = x
  , sendMessageParseMode = Nothing
  , sendMessageDisableWebPagePreview = Nothing
  , sendMessageDisableNotification = Nothing
  , sendMessageReplyToMessageId = Nothing
  , sendMessageReplyMarkup = Nothing
  }

main :: IO ()
main = (Token . T.pack <$> getEnv "TOKEN") >>= startWebhook

startWebhook :: Token -> IO ()
startWebhook (Token token) = do
  env <- defaultTelegramClientEnv (Token token)
  blacklistsRef <- newIORef HashMap.empty
  runEnv 3000 $ \req respond -> do
    if pathInfo req == ["bot" <> token]
      then do
        mupdate <- decode <$> strictRequestBody req
        forM_ mupdate $ \update ->
          forkIO $ handleUpdate update env blacklistsRef
        respond $ responseLBS status200 [] ""
      else
        respond $ responseLBS status404 [] ""

mention :: UserId -> Text -> Text
mention (UserId userId) content =
  "<a href=\"tg://user?id=" <> T.pack (show userId) <> "\">" <> content <> "</a>"

escape :: Text -> Text
escape =
    T.replace "&" "&amp;"
  . T.replace "<" "&lt;"
  . T.replace ">" "&gt;"
