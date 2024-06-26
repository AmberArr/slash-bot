{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson (decode)
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
import Network.Wai.Handler.WarpTLS as Warp
import Servant.Client
import System.Environment
import qualified Telegram.Bot.API as Tg
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.Logger
import qualified Text.Regex.TDFA as Regex
import Database.Persist.Sql (runMigration)

import Db
import Bot
import Config
import Interface
import Parser
import Util
import qualified Blacklist (migrateAll)

data Action =
    Ping
  | BlackListAdd ![Text]
  | BlackListDel ![Text]
  | BlackListAddRegex !Text
  | BlackListDelRegex !Text
  | BlackListList
  | CheckBlackList !Text
  | Reply !Text
  deriving (Show, Eq)

handleUpdate :: (WithBlacklist m, WithBot r m, MonadIO m)
             => Tg.Update -> m ()
handleUpdate update = void $ runMaybeT $ do
  botUsername <- asks (botCfgUsername . getter)
  chatId <- hoistMaybe $ Tg.updateChatId update
  cmdInfo <- fmap eitherToMaybe $ runExceptT $ parseUpdate update botUsername
  let actions = actionRoute cmdInfo
  message <- hoistMaybe $ update & Tg.updateMessage
  let messageId = Tg.messageMessageId message
  lift $ handleEffectful chatId messageId actions

actionRoute :: Maybe CmdInfo -> [Action]
actionRoute Nothing = fail ""
actionRoute (Just CmdInfo{..}) = do
  if isActiveVoice
    then activeVoiceHandler (cmd, remainder)
    else passiveVoiceHandler (cmd, remainder)
  where
    activeVoiceHandler = \case
      ("_ping", _)                    -> pure Ping
      ("_blacklist", "add" : xs)      -> pure $ BlackListAdd xs
      ("_blacklist", "del" : xs)      -> pure $ BlackListDel xs
      ("_blacklist", "addregex" : xs) -> pure $ BlackListAddRegex $ T.drop 21 rawinput  -- it works, for now
      ("_blacklist", "delregex" : xs) -> pure $ BlackListDelRegex $ T.drop 21 rawinput
      ("_blacklist", [])              -> pure BlackListList
      (_, T.unwords -> predicate)
        | Just RequestingMode <- altMode -> pure $ Reply [i|#{subject} 叫 #{recipient} #{predicate}！|]
      x | isIgnoreBlacklist -> continue x
      x@(cmd, _) -> CheckBlackList ("/" <> cmd) : continue x
    continue = \case
      ("me", [])                      -> fail ""
      ("you", [])                     -> fail ""
      ("me", T.unwords -> predicate)  -> pure $ Reply [i|#{subject} #{predicate}！|]
      ("you", T.unwords -> predicate) -> pure $ Reply [i|#{recipient} #{predicate}！|]
      (verb, [])                      -> pure $ Reply [i|#{subject} #{verb} 了 #{recipient}！|]
      (verb, T.unwords -> patient)    -> pure $ Reply [i|#{subject} #{verb} #{recipient} #{patient}！|]
    passiveVoiceHandler = \case
      (_, T.unwords -> predicate)
        | Just RequestingMode <- altMode -> pure $ Reply [i|#{recipient} 叫 #{subject} #{predicate}！|]
      x@(cmd, _)                       -> CheckBlackList ("\\" <> cmd) : continue2 x
    continue2 = \case
      (verb, [])                      -> pure $ Reply [i|#{subject} 被 #{recipient} #{verb} 了！|]
      (verb , T.unwords -> patient)   -> pure $ Reply [i|#{subject} 被 #{recipient} #{verb}#{patient}！|]

handleEffectful :: (WithBlacklist m, WithBot r m)
                => Tg.ChatId -> Tg.MessageId -> [Action] -> m ()
handleEffectful chatId messageId [] = pure ()
handleEffectful chatId messageId (action : actions) =
  case action of
    CheckBlackList cmd -> do
      b <- checkBlacklist chatId cmd
      unless b $ handleEffectful chatId messageId actions
    _ -> continue action >> handleEffectful chatId messageId actions
  where
    continue = \case
      Ping -> reply' "111"
      BlackListAdd xs -> do
        forM_ xs $ addBlacklistItem chatId
        reply' "ok"
      BlackListDel xs -> do
        forM_ xs $ delBlacklistItem chatId
        reply' "ok"
      BlackListList -> do
        (plains, regexs) <- getBlacklist chatId
        reply' $ T.unlines $
          [ "Plaintext blacklist:"
          , T.intercalate " " (Set.toList plains)
          , ""
          , "Regex blacklist:"
          ]
          <> fmap codeMarkup regexs
      BlackListAddRegex txt -> case unFail $ Regex.makeRegexM $ T.unpack txt of
        Right (regex :: Regex.Regex) -> addBlacklistItem chatId (BLRegex txt) >> reply' "ok"
        Left err -> reply' $ "Invalid regex: " <> T.pack err
      BlackListDelRegex txt -> delBlacklistItem chatId (BLRegex txt) >> reply' "ok"
      Reply x -> reply' x
      CheckBlackList cmd -> undefined
    reply' x = sendTextTo (Tg.SomeChatId chatId) (Just messageId) $ prependLTRMark x
    prependLTRMark x = "\8206" <> x


-- there's no MonadFail for (Either String) ...
newtype Fail a = Fail { unFail :: Either String a }
  deriving (Functor, Applicative, Monad)

instance MonadFail Fail where
  fail x = Fail (Left x)

main :: IO ()
main = do
  polling <- isJust <$> lookupEnv "POLLING"
  token <- Tg.Token . T.pack <$> getEnv "TOKEN"
  clientEnv <- Tg.defaultTelegramClientEnv token
  Just botUsername <- Tg.userUsername <$>
    runReaderT (runTgApi_ Tg.getMe) clientEnv

  dburl <- T.pack <$> getEnv "DATABASE_URL"
  runNoLoggingT $ withDBConn dburl $ \conn -> do
    let env = (BotConfig (Just Tg.HTML) botUsername, clientEnv, conn)
    liftIO $ flip runReaderT conn $ runMigration Blacklist.migrateAll
    if polling
      then liftIO $ startPolling' env handleUpdate
      else liftIO $ startWebhook env token handleUpdate

startPolling' :: Env -> (Tg.Update -> BotM a) -> IO ()
startPolling' env handler =
  controlT (\run -> startPolling $ run . handler) `runBotM_` env

-- Steal from Telegram.Bot.Simple.BotApp.Internal
startPolling :: (Tg.Update -> ClientM a) -> ClientM a
startPolling handleUpdate = go Nothing
  where
    go lastUpdateId = do
      let inc (Tg.UpdateId n) = Tg.UpdateId (n + 1)
          offset = fmap inc lastUpdateId
      res <-
        (Right <$> Tg.getUpdates
          (Tg.GetUpdatesRequest offset Nothing (Just 25) Nothing))
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

startWebhook :: Env -> Tg.Token -> (Tg.Update -> BotM a)  -> IO ()
startWebhook env (Tg.Token token) handler0 = do
  runServer <- initWebhookServer
  runServer $ \req respond -> do
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

initWebhookServer :: IO (Network.Wai.Application -> IO ())
initWebhookServer = do
  port <- lookupEnv "PORT" >>= maybe (pure 3000) runReadPort
  maybeCert <- lookupEnv "SERVER_CERT"
  maybeKey <- lookupEnv "SERVER_KEY"
  pure $ case (maybeCert, maybeKey) of
    (Just cert, Just key) -> let tlsSetting = Warp.tlsSettings cert key
                                 setting = Warp.setPort port Warp.defaultSettings
                              in Warp.runTLS tlsSetting setting
    _                     -> Warp.runEnv port
  where
    runReadPort :: String -> IO Int
    runReadPort sp = case reads sp of
        ((p', _):_) -> pure p'
        _ -> fail $ "Invalid value in $PORT: " ++ sp


eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a
