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
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Servant.Client
import System.Environment
import qualified Telegram.Bot.API as Tg
import qualified Telegram.Bot.Simple.UpdateParser as P
import Control.Monad.Except
import Control.Monad.Trans.Control
import Network.HTTP.Simple
import Control.Lens
import Text.XML.Lens
import qualified Text.HTML.DOM as HTML

import Bot
import Config
import Db (DBConn, mkTable, withDBConn)
import Interface
import Parser

data Action =
    Ping
  | BlackListAdd ![Text]
  | BlackListDel ![Text]
  | BlackListList
  | Reply !Text
  deriving (Show, Eq)

mkEnv :: BotConfig -> ClientEnv -> DBConn -> Env
mkEnv x y conn = (x, y, conn)

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
        | cmd `Set.member` blacklist  -> fail ""
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

  dburl <- BC.pack <$> getEnv "DATABASE_URL"
  withDBConn dburl $ \conn -> do
    mkTable conn
    let env = mkEnv (BotConfig (Just Tg.HTML) botUsername) clientEnv conn
    if polling
      then (=<<) (either print (const $ pure ())) $ flip runBotM env $ startPolling' handleUpdate
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


eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a
