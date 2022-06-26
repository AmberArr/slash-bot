{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Monad.Except
import Control.Monad.Trans.Control

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
{- HLINT ignore handleUpdate -}
handleUpdate update = void $ runMaybeT $ do
  botUsername <- botCfgUsername <$> asks getter
  chatId <- MaybeT $ pure $ Tg.updateChatId update
  action <- MaybeT $ handle1 update botUsername <$> getBlacklist chatId
  message <- MaybeT $ pure $ update & Tg.updateMessage
  let messageId = Tg.messageMessageId message
  lift $ handleEffectful chatId messageId action

handle1 :: Tg.Update -> Text -> Blacklist -> Maybe Action
handle1 update botUsername blacklist = do
  text <- P.runUpdateParser P.text update

  let words = T.words $ T.tail $ escape $ stripBotName text

  (senderName, senderLinkBuilder) <-
    update & (Tg.updateMessage >=> getSenderFromMessage)


  let sender = maybe "Ta" senderLinkBuilder senderName

      maybeRecipient =
        update & (Tg.updateMessage
              >=> Tg.messageReplyToMessage
              >=> getSenderFromMessage)

      recipient = case maybeRecipient of
        Just (Just recipientName, recipientLinkBuilder) ->
          recipientLinkBuilder recipientName
        Nothing ->
          senderLinkBuilder "自己"

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

  case T.head text of
    '/' -> activeVoiceHandler words
    '\\' -> passiveVoiceHandler words
    _ -> fail "not a command"
  where
    stripBotName text = case T.words text of
      ((T.stripSuffix ("@" <> botUsername) -> Just x) : xs) -> T.unwords (x:xs)
      _ -> text

-- return sender name and mention link builder (if it can)
getSenderFromMessage :: Tg.Message -> Maybe (Maybe Text, Text -> Text)
getSenderFromMessage msg = do
  from <- Tg.messageFrom msg
  Tg.UserId userId <- pure $ Tg.userId from
  case userId of
    -- 136817688: if sender is "@Channel_Bot"
    -- 1087968824: if sender is "@GroupAnonymousBot"
    x | x == 136817688 || x == 1087968824 -> do
      senderChat <- Tg.messageSenderChat msg
      let name = Tg.chatTitle senderChat
      username <- Tg.chatUsername senderChat
      pure (name, mentionWithUsername username)
    realUserId -> do
      let name = Tg.userFirstName from
      pure (Just name, mentionWithId (Tg.UserId realUserId))

mentionWithId :: Tg.UserId -> Text -> Text
mentionWithId (Tg.UserId userId) content =
  "<a href=\"tg://user?id=" <> T.pack (show userId) <> "\">" <> content <> "</a>"

mentionWithUsername :: Text -> Text -> Text
mentionWithUsername username content =
  "<a href=\"https://t.me/" <> username <> "\">" <> content <> "</a>"

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
  polling <- isJust <$> lookupEnv "POLLING"
  token <- Tg.Token . T.pack <$> getEnv "TOKEN"
  clientEnv <- Tg.defaultTelegramClientEnv token
  Just botUsername <- Tg.userUsername <$>
    runReaderT (runTgApi_ Tg.getMe) clientEnv

  dburl <- BC.pack <$> getEnv "DATABASE_URL"
  withDBConn dburl $ \conn -> do
    mkTable conn
    let env = mkEnv (BotConfig (Just Tg.HTML) botUsername) clientEnv conn
    if polling
      then flip runBotM_ env $ startPolling' handleUpdate
      else startWebhook token $ flip runBotM_ env . handleUpdate

startPolling' :: (Tg.Update -> BotM a) -> BotM a
startPolling' handler =
  controlT (\run -> startPolling $ run . handler)

startPolling :: (Tg.Update -> ClientM a) -> ClientM a
startPolling handleUpdate = go Nothing
  where
    go lastUpdateId = do
      let inc (Tg.UpdateId n) = Tg.UpdateId (n + 1)
          offset = fmap inc lastUpdateId
      res <-
        (Right <$> Tg.getUpdates
          (Tg.GetUpdatesRequest offset Nothing Nothing Nothing))
        `catchError` (pure . Left)

      nextUpdateId <- case res of
        Left servantErr -> do
          liftIO (print servantErr)
          pure lastUpdateId
        Right result -> do
          let updates = Tg.responseResult result
              updateIds = map Tg.updateUpdateId updates
              maxUpdateId = maximum (Nothing : map Just updateIds)
          mapM_ handleUpdate updates
          pure maxUpdateId
      liftIO $ threadDelay 1000000
      go nextUpdateId

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

escape :: Text -> Text
escape =
    T.replace "&" "&amp;"
  . T.replace "<" "&lt;"
  . T.replace ">" "&gt;"

