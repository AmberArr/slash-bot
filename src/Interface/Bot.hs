{-# LANGUAGE TypeFamilies #-}

module Interface.Bot where

import Control.Monad

import Data.Text (Text)
import Telegram.Bot.API qualified as Tg

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Fail
import Effectful.Labeled
import Effectful.Reader.Static
import Servant.Client (ClientEnv, ClientError, ClientM, runClientM)

import Config

data Telegram :: Effect where
  RunTelegram :: Maybe (ClientError -> b) -> ClientM a -> Telegram m (Either b a)
  GetUsername :: Telegram m Text
type instance DispatchOf Telegram = Dynamic

runTelegramContext
  :: (Fail :> es, IOE :> es)
  => Tg.Token
  -> Eff (Telegram ': es) a
  -> Eff es (Either ClientError a)
runTelegramContext token = reinterpret localEff $ \_ -> \case
  RunTelegram (Just handler) clientm -> (Right <$> clientMToEff clientm) `catchError` (\_ err -> pure $ Left $ handler err)
  RunTelegram Nothing clientm -> Right <$> clientMToEff clientm
  GetUsername -> labeled @"username" @(Reader Text) ask
  where
    localEff
      :: (Fail :> es, IOE :> es)
      => Eff (Labeled "username" (Reader Text) : Labeled "env" (Reader ClientEnv) : Error ClientError : es) a
      -> Eff es (Either ClientError a)
    localEff eff = do
      clientEnv <- liftIO $ Tg.defaultTelegramClientEnv token
      runErrorNoCallStack @ClientError $ runLabeled @"env" (runReader clientEnv) $ do
        musername <- Tg.userUsername . Tg.responseResult <$> clientMToEff Tg.getMe
        username <- maybe (fail "cannot fetch username") pure musername
        runLabeled @"username" (runReader username) eff

{-# INLINE runTelegramMethod #-}
runTelegramMethod :: (HasCallStack, Telegram :> es) => ClientM (Tg.Response a) -> Eff es a
runTelegramMethod = fmap (either undefined Tg.responseResult) . send . RunTelegram Nothing

{-# INLINE runTelegramMethodEither #-}
runTelegramMethodEither
  :: (HasCallStack, Telegram :> es)
  => ClientM (Tg.Response a)
  -> Eff es (Either ClientError a)
runTelegramMethodEither clientm = fmap Tg.responseResult <$> send (RunTelegram (Just id) clientm)

{-# INLINE getUsername #-}
getUsername :: (HasCallStack, Telegram :> es) => Eff es Text
getUsername = send GetUsername

clientMToEff
  :: (Error ClientError :> es, IOE :> es, Labeled "env" (Reader ClientEnv) :> es)
  => ClientM a
  -> Eff es a
clientMToEff clientm = do
  clientEnv <- labeled @"env" @(Reader ClientEnv) $ ask
  result <- liftIO $ runClientM clientm clientEnv
  either throwError pure result

sendTextTo
  :: (Labeled "config" (Reader BotConfig) :> es, Telegram :> es)
  => Tg.SomeChatId
  -> Maybe Tg.MessageId
  -> Text
  -> Eff es ()
sendTextTo chatId messageId text = do
  parseMode <- labeled @"config" @(Reader BotConfig) $ asks botCfgParseMode
  void $
    runTelegramMethod $
      Tg.sendMessage $
        (Tg.defSendMessage chatId text)
          { Tg.sendMessageParseMode = parseMode
          , Tg.sendMessageLinkPreviewOptions = Just linkPreviewOptions
          , Tg.sendMessageReplyToMessageId = messageId
          }
  where
    linkPreviewOptions =
      Tg.LinkPreviewOptions
        { Tg.linkPreviewOptionsIsDisabled = Just True
        , Tg.linkPreviewOptionsUrl = Nothing
        , Tg.linkPreviewOptionsPreferSmallMedia = Nothing
        , Tg.linkPreviewOptionsPreferLargeMedia = Nothing
        , Tg.linkPreviewOptionsShowAboveText = Nothing
        }
