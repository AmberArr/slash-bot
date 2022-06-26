{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Interface.Bot where

import Control.Monad.Reader
import Data.Function
import Data.Has
import Data.Text (Text)
import Servant.Client
import qualified Telegram.Bot.API as Tg

import Config

type WithBot env m = MonadBot env m

class (Monad m, MonadReader r m, Has BotConfig r) => MonadBot r m where
  getMe :: m Tg.User
  sendMessage :: Tg.SendMessageRequest -> m Tg.Message

{- HLINT ignore sendTextTo -}
sendTextTo :: MonadBot r m
           => Tg.SomeChatId -> Maybe Tg.MessageId -> Text -> m ()
sendTextTo chatId messageId text = do
  parseMode <- botCfgParseMode <$> asks getter
  void $ sendMessage Tg.SendMessageRequest
    { Tg.sendMessageChatId = chatId
    , Tg.sendMessageText = text
    , Tg.sendMessageParseMode = parseMode
    , Tg.sendMessageEntities = Nothing
    , Tg.sendMessageDisableWebPagePreview = Nothing
    , Tg.sendMessageDisableNotification = Nothing
    , Tg.sendMessageProtectContent = Nothing
    , Tg.sendMessageReplyToMessageId = messageId
    , Tg.sendMessageAllowSendingWithoutReply = Nothing
    , Tg.sendMessageReplyMarkup = Nothing
    }

reply :: MonadBot r m
      => Tg.Update -> Text -> m ()
reply update text = sendTextTo (Tg.SomeChatId chatId) messageId text
  where
    messageId = update & (Tg.updateMessage
                      >=> pure . Tg.messageMessageId)
    Just chatId = Tg.updateChatId update
