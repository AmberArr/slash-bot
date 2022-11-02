{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Arrow ((>>>), (&&&), second)
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
import qualified Data.Text.Encoding as T (decodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Servant.Client
import System.Environment
import qualified Telegram.Bot.API as Tg
import Telegram.Bot.Simple.BotApp.Internal (startPolling)
import qualified Telegram.Bot.Simple.UpdateParser as P
import Control.Monad.Except
import Control.Monad.Trans.Control
import Network.HTTP.Simple
import Database.Persist.Sqlite (withSqliteConn)
import Control.Monad.Logger

import Bot
import Config
import Interface
import Parser

data Action =
    Ping
  | BlackListAdd ![Text]
  | BlackListDel ![Text]
  | BlackListList
  | Reply !Text
  deriving (Show, Eq)

handleUpdate :: (WithBlacklist m, WithBot r m, MonadIO m)
             => Tg.Update -> m ()
{- HLINT ignore handleUpdate -}
handleUpdate update = void $ runMaybeT $ do
  botUsername <- botCfgUsername <$> asks getter
  chatId <- hoistMaybe $ Tg.updateChatId update
  cmdInfo <- fmap eitherToMaybe $ runExceptT $ parseUpdate update botUsername
  action <- MaybeT $ actionRoute cmdInfo <$> getBlacklist chatId
  message <- hoistMaybe $ update & Tg.updateMessage
  let messageId = Tg.messageMessageId message
  lift $ handleEffectful chatId messageId action

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

actionRoute :: Maybe CmdInfo -> Blacklist -> Maybe Action
actionRoute Nothing _ = Nothing
actionRoute (Just CmdInfo{..}) blacklist = do
  if isActiveVoice
    then activeVoiceHandler (cmd, remainder)
    else passiveVoiceHandler (cmd, remainder)
  where
    activeVoiceHandler = \case
      ("_ping", _)                    -> pure Ping
      ("_blacklist", "add" : xs)      -> pure $ BlackListAdd xs
      ("_blacklist", "del" : xs)      -> pure $ BlackListDel xs
      ("_blacklist", [])              -> pure BlackListList
      (cmd, _)
        | not isIgnoreBlacklist
        && cmd `Set.member` blacklist -> fail ""
      ("me", [])                      -> fail ""
      ("you", [])                     -> fail ""
      ("me", T.unwords -> predicate)  -> pure $ Reply [i|#{subject} #{predicate}！|]
      ("you", T.unwords -> predicate) -> pure $ Reply [i|#{recipient} #{predicate}！|]
      (_, words) | isAltSubject       -> pure $ Reply $ T.unwords (subject : words) <> "！"
      (verb, [])                      -> pure $ Reply [i|#{subject} #{verb} 了 #{recipient}！|]
      (verb, T.unwords -> patient)    -> pure $ Reply [i|#{subject} #{verb} #{recipient} #{patient}！|]
    passiveVoiceHandler = \case
      (verb, [])                      -> pure $ Reply [i|#{subject} 被 #{recipient} #{verb} 了！|]
      (verb , T.unwords -> patient)   -> pure $ Reply [i|#{subject} 被 #{recipient} #{verb}#{patient}！|]

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

  dburl <- T.pack <$> getEnv "DATABASE_URL"
  runNoLoggingT $ withSqliteConn dburl $ \conn -> do
    let env = (BotConfig (Just Tg.HTML) botUsername, clientEnv, conn)
    if polling
      then liftIO $ startPolling' env handleUpdate
      else liftIO $ startWebhook env token handleUpdate

startPolling' :: Env -> (Tg.Update -> BotM a) -> IO ()
startPolling' env handler =
  controlT (\run -> startPolling $ run . handler) `runBotM_` env

startWebhook :: Env -> Tg.Token -> (Tg.Update -> BotM a)  -> IO ()
startWebhook env (Tg.Token token) handler0 = do
  Warp.runEnv 3000 $ \req respond -> do
    if pathInfo req == ["bot" <> token]
      then do
        mupdate <- decode <$> strictRequestBody req
        forM_ mupdate $ \update ->
          forkIO $ handler update
        respond $ responseLBS status200 [] ""
      else
        respond $ responseLBS status404 [] ""
  where
      handler x = handler0 x `runBotM_` env


eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a
