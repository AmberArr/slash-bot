{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader qualified as Transformer
import Data.Aeson (decode)
import Data.Maybe
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Persist.Sql (runMigration)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WarpTLS as Warp
import System.Environment
import Telegram.Bot.API qualified as Tg
import Text.Regex.TDFA qualified as Regex

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Fail
import Effectful.Labeled
import Effectful.Reader.Static

import Blacklist qualified (migration)
import Config
import Db
import Interface
import Parser
import Util

data Action :: Effect where
  Ping :: Action m ()
  BlackListAdd :: [Text] -> Action m ()
  BlackListDel :: [Text] -> Action m ()
  BlackListAddRegex :: Text -> Action m ()
  BlackListDelRegex :: Text -> Action m ()
  BlackListList :: Action m ()
  CheckBlackList :: Text -> Action m ()
  Reply :: Text -> Action m ()
type instance DispatchOf Action = Dynamic

handleUpdate
  :: ( Fail :> es
     , IOE :> es
     , Labeled "config" (Reader BotConfig) :> es
     , Telegram :> es
     , WithBlacklist :> es
     )
  => Tg.Update
  -> Eff es ()
handleUpdate update = void $ do
  botUsername <- getUsername
  Just chatId <- pure $ Tg.updateChatId update
  cmdInfo <- parseUpdate update botUsername
  Just message <- pure $ Tg.updateMessage update
  let messageId = Tg.messageMessageId message
  runAction chatId messageId $ actionRoute cmdInfo

actionRoute :: (Action :> es, Fail :> es) => CmdInfo -> Eff es ()
actionRoute info@CmdInfo{..} = do
  if isActiveVoice
    then activeVoiceHandler info (cmd, remainder)
    else passiveVoiceHandler info (cmd, remainder)

activeVoiceHandler :: (Action :> es, Fail :> es) => CmdInfo -> (Text, [Text]) -> Eff es ()
activeVoiceHandler CmdInfo{..} = \case
  ("_ping", _) -> send Ping
  ("_blacklist", "add" : xs) -> send $ BlackListAdd xs
  ("_blacklist", "del" : xs) -> send $ BlackListDel xs
  ("_blacklist", "addregex" : xs) -> send $ BlackListAddRegex $ T.drop 21 rawinput -- it works, for now
  ("_blacklist", "delregex" : xs) -> send $ BlackListDelRegex $ T.drop 21 rawinput
  ("_blacklist", []) -> send BlackListList
  (_, T.unwords -> predicate)
    | Just RequestingMode <- altMode -> send $ Reply [i|#{subject} 叫 #{recipient} #{predicate}！|]
  x | isIgnoreBlacklist -> continue x
  x@(cmd, _) -> do
    send (CheckBlackList ("/" <> cmd)) >> continue x
  where
    continue = \case
      ("me", []) -> fail ""
      ("you", []) -> fail ""
      ("me", T.unwords -> predicate) -> send $ Reply [i|#{subject} #{predicate}！|]
      ("you", T.unwords -> predicate) -> send $ Reply [i|#{recipient} #{predicate}！|]
      (verb, []) -> send $ Reply [i|#{subject} #{verb} 了 #{recipient}！|]
      (verb, T.unwords -> patient) -> send $ Reply [i|#{subject} #{verb} #{recipient} #{patient}！|]

passiveVoiceHandler :: (Action :> es, Fail :> es) => CmdInfo -> (Text, [Text]) -> Eff es ()
passiveVoiceHandler CmdInfo{..} = \case
  (_, T.unwords -> predicate)
    | Just RequestingMode <- altMode -> send $ Reply [i|#{recipient} 叫 #{subject} #{predicate}！|]
  x@(cmd, _) -> send (CheckBlackList ("\\" <> cmd)) >> continue x
  where
    continue = \case
      (verb, []) -> send $ Reply [i|#{subject} 被 #{recipient} #{verb} 了！|]
      (verb, T.unwords -> patient) -> send $ Reply [i|#{subject} 被 #{recipient} #{verb}#{patient}！|]

runAction
  :: (WithBlacklist :> es, Telegram :> es, Labeled "config" (Reader BotConfig) :> es, Fail :> es)
  => Tg.ChatId
  -> Tg.MessageId
  -> Eff (Action ': es) a
  -> Eff es a
runAction chatId messageId = interpret $ \_ -> \case
  CheckBlackList cmd ->
    checkBlacklist chatId cmd >>= \b -> if b then pure () else fail ""
  Ping -> reply "111"
  BlackListAdd xs -> do
    forM_ xs $ addBlacklistItem chatId
    reply "ok"
  BlackListDel xs -> do
    forM_ xs $ delBlacklistItem chatId
    reply "ok"
  BlackListList -> do
    (plains, regexs) <- getBlacklist chatId
    reply $
      T.unlines $
        [ "Plaintext blacklist:"
        , T.intercalate " " (Set.toList plains)
        , ""
        , "Regex blacklist:"
        ]
          <> fmap codeMarkup regexs
  BlackListAddRegex txt -> case runPureEff $ runFail $ Regex.makeRegexM $ T.unpack txt of
    Right (regex :: Regex.Regex) -> addBlacklistItem chatId (BLRegex txt) >> reply "ok"
    Left err -> reply $ "Invalid regex: " <> T.pack err
  BlackListDelRegex txt -> delBlacklistItem chatId (BLRegex txt) >> reply "ok"
  Reply x -> reply x
  where
    reply :: (Telegram :> es, Labeled "config" (Reader BotConfig) :> es) => Text -> Eff es ()
    reply x = sendTextTo (Tg.SomeChatId chatId) (Just messageId) $ prependLTRMark x
    prependLTRMark x = "\8206" <> x

main :: IO ()
main = do
  polling <- isJust <$> lookupEnv "POLLING"
  token <- Tg.Token . T.pack <$> getEnv "TOKEN"

  dburl <- T.pack <$> getEnv "DATABASE_URL"
  runNoLoggingT $ withDBConn dburl $ \conn -> do
    liftIO $ do
      flip Transformer.runReaderT conn $ runMigration Blacklist.migration
      print =<<
        ( runEff
            . runFail
            . runTelegramContext token
            . runWithBlacklist conn
            . runLabeled @"config" (runReader (BotConfig (Just Tg.HTML)))
        )
          (if polling then startPolling handleUpdate else startWebhook token handleUpdate)

type Effects = [Labeled "config" (Reader BotConfig), WithBlacklist, Telegram, Fail, IOE]

startWebhook :: (Effects ~ es) => Tg.Token -> (Tg.Update -> Eff es b) -> Eff es ()
startWebhook (Tg.Token token) handleUpdate = do
  runServer <- liftIO initWebhookServer
  withSeqEffToIO $ \run -> do
    runServer $ \req respond -> do
      if pathInfo req == ["bot" <> token]
        then do
          mupdate <- decode <$> strictRequestBody req
          forM_ mupdate $ \update ->
            run $ handleUpdate update
          respond $ responseLBS status200 [] ""
        else respond $ responseLBS status404 [] ""

startPolling :: (Effects ~ es) => (Tg.Update -> Eff es b) -> Eff es ()
startPolling handleUpdate = go Nothing
  where
    go lastUpdateId = do
      let inc (Tg.UpdateId n) = Tg.UpdateId (n + 1)
          offset = fmap inc lastUpdateId
      res <-
        runTelegramMethodEither $
          Tg.getUpdates (Tg.GetUpdatesRequest offset Nothing (Just 25) Nothing)

      nextUpdateId <- case res of
        Left servantErr -> do
          liftIO $ print servantErr
          pure lastUpdateId
        Right updates -> do
          let updateIds = map Tg.updateUpdateId updates
              maxUpdateId = maximum (Nothing : map Just updateIds)
          mapM_ handleUpdate updates
          pure maxUpdateId
      liftIO $ threadDelay 1000000
      go nextUpdateId

initWebhookServer :: IO (Network.Wai.Application -> IO ())
initWebhookServer = do
  port <- lookupEnv "PORT" >>= maybe (pure 3000) runReadPort
  maybeCert <- lookupEnv "SERVER_CERT"
  maybeKey <- lookupEnv "SERVER_KEY"
  pure $ case (maybeCert, maybeKey) of
    (Just cert, Just key) ->
      let tlsSetting = Warp.tlsSettings cert key
          setting = Warp.setPort port Warp.defaultSettings
      in Warp.runTLS tlsSetting setting
    _ -> Warp.runEnv port
  where
    runReadPort :: String -> IO Int
    runReadPort sp = case reads sp of
      ((p', _) : _) -> pure p'
      _ -> fail $ "Invalid value in $PORT: " ++ sp
