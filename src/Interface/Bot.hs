{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Interface.Bot where

import Control.Monad
import Control.Monad.Reader
import Data.Function
import Data.Has
import Data.Text (Text)
import qualified Telegram.Bot.API as Tg

import Config

type WithBot env m = MonadBot env m

class (Monad m, MonadReader r m, Has BotConfig r) => MonadBot r m where
  getMe :: m Tg.User
  sendMessage :: Tg.SendMessageRequest -> m Tg.Message

sendTextTo :: MonadBot r m
           => Tg.SomeChatId -> Maybe Tg.MessageId -> Text -> m ()
sendTextTo chatId messageId text = do
  parseMode <- asks (botCfgParseMode . getter)
  void $ sendMessage $ (Tg.defSendMessage chatId text)
    { Tg.sendMessageParseMode = parseMode
    , Tg.sendMessageLinkPreviewOptions = Just linkPreviewOptions
    , Tg.sendMessageReplyToMessageId = messageId
    }
  where
    linkPreviewOptions = Tg.LinkPreviewOptions
      { Tg.linkPreviewOptionsIsDisabled       = Just True
      , Tg.linkPreviewOptionsUrl              = Nothing
      , Tg.linkPreviewOptionsPreferSmallMedia = Nothing
      , Tg.linkPreviewOptionsPreferLargeMedia = Nothing
      , Tg.linkPreviewOptionsShowAboveText    = Nothing
      }

reply :: MonadBot r m
      => Tg.Update -> Text -> m ()
reply update text = sendTextTo (Tg.SomeChatId chatId) messageId text
  where
    messageId = update & (Tg.updateMessage
                      >=> pure . Tg.messageMessageId)
    Just chatId = Tg.updateChatId update
