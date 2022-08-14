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

import Control.Arrow ((&&&), second)
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
import qualified Telegram.Bot.Simple.UpdateParser as P
import Control.Monad.Except
import Control.Monad.Trans.Control
import Network.HTTP.Simple

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

data CmdInfo = CmdInfo
  { cmd :: Text
  , sender :: Text
  , recipient :: Text
  , remainder :: [Text]
  , isActiveVoice :: Bool
  } deriving (Show, Eq)

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

liftMaybe :: MonadError () m => Maybe a -> m a
liftMaybe = maybe (throwError ()) pure

parseUpdate :: (MonadError () m, MonadIO m) => Tg.Update -> Text -> m CmdInfo
parseUpdate update botUsername = do
  text <- liftMaybe $ P.runUpdateParser P.text update
  isActiveVoice <- case T.head text of
    '/' -> pure True
    '\\' -> pure False
    _ -> throwError ()

  let words = T.words $ T.tail $ escape text
  ((cmd, username0), remainder0) <- case words of
    [] -> throwError ()
    (head : tail) -> pure (splitAtAt head, tail)

  -- It's kinda dumb. But whatever, it works.
  let checkSecondWord = \case
        (secondWord : rem) | "@" `T.isPrefixOf` secondWord ->
          let username = T.tail secondWord
           in (\x -> (Just x, username, rem)) <$> fetchFirstName username
        _ ->
          pure (Nothing, username0, remainder0)

  (maybeRecipient0, username, remainder) <- if
    | T.null username0 ->
        checkSecondWord remainder0
    | username0 == botUsername ->
        checkSecondWord remainder0
    | "bot" `T.isSuffixOf` T.toLower username0 ->
        throwError ()
    | otherwise ->
        (\x -> (Just x, username0, remainder0)) <$> fetchFirstName username0

  liftMaybe $ do
    (senderName, senderLinkBuilder) <-
      update & (Tg.updateMessage >=> getSenderFromMessage)

    let sender = maybe "Ta" senderLinkBuilder senderName
        f = \x -> (Just x, mentionWithUsername username)
        maybeRecipient = msum
          [ fmap f maybeRecipient0
          , update & (Tg.updateMessage
                  >=> Tg.messageReplyToMessage
                  >=> getSenderFromMessage)
          ]
        recipient = case maybeRecipient of
          Just (Just recipientName, recipientLinkBuilder) ->
            recipientLinkBuilder recipientName
          _ ->
            senderLinkBuilder "自己"
    pure CmdInfo{..}

fetchFirstName :: MonadIO m => Text -> m Text
fetchFirstName username = do
  let req = parseRequest_ $ T.unpack $ toTgUserWebLink username
  resp <- liftIO $ httpBS req
  pure $ extractFromHTML $ T.decodeUtf8 $ getResponseBody resp

extractFromHTML :: Text -> Text
extractFromHTML =
    unescape
  . T.takeWhile (/= '<')
  . snd
  . T.breakOnEnd "<div class=\"tgme_page_title\"><span dir=\"auto\">"

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
      ("me", T.unwords -> predicate)  -> pure $ Reply [i|#{sender} #{predicate}！|]
      ("you", T.unwords -> predicate) -> pure $ Reply [i|#{recipient} #{predicate}！|]
      (verb, [])                      -> pure $ Reply [i|#{sender} #{verb} 了 #{recipient}！|]
      (verb, T.unwords -> patient)    -> pure $ Reply [i|#{sender} #{verb} #{recipient} #{patient}！|]
    passiveVoiceHandler = \case
      (verb, [])                      -> pure $ Reply [i|#{sender} 被 #{recipient} #{verb} 了！|]
      (verb , T.unwords -> patient)   -> pure $ Reply [i|#{sender} 被 #{recipient} #{verb}#{patient}！|]

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
  [i|<a href="tg://user?id=#{userId}">#{content}</a>|]

mentionWithUsername :: Text -> Text -> Text
mentionWithUsername username content =
  [i|<a href="#{toTgUserLink username}">#{content}</a>|]

toTgUserLink :: Text -> Text
toTgUserLink username = "tg://resolve?domain=" <> username

toTgUserWebLink :: Text -> Text
toTgUserWebLink username = "https://t.me/" <> username

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

escape :: Text -> Text
escape =
    T.replace "&" "&amp;"
  . T.replace "<" "&lt;"
  . T.replace ">" "&gt;"
  . T.replace "\"" "&quot;"
  . T.replace "'"  "&#39;"

unescape :: Text -> Text
unescape =
    T.replace "&amp;"   "&"
  . T.replace "&lt;"    "<"
  . T.replace "&gt;"    ">"
  . T.replace "&quot;"  "\""
  . T.replace "&#39;"   "'"

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

splitAtAt :: Text -> (Text, Text)
splitAtAt text =
  case T.break (== '@') text of
    (x, y) | "@" `T.isPrefixOf` y -> (x, T.tail y)
    x -> x

