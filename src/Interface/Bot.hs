{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Interface.Bot where

import Control.Monad.Reader
import Data.Function
import Data.Has
import Data.Text (Text)
import Servant.Client
import qualified Telegram.Bot.API as Tg

import Config

type WithBot env m = (MonadBot m, MonadReader env m, Has BotConfig env)

class Monad m => MonadBot m where
  getMe :: m Tg.User
  sendMessage :: Tg.SendMessageRequest -> m Tg.Message

reply :: (MonadBot m, MonadReader r m, Has BotConfig r)
      => Tg.Update -> Text -> m ()
reply update text = do
  parseMode <- botCfgParseMode <$> asks getter
  void $ sendMessage Tg.SendMessageRequest
    { Tg.sendMessageChatId = Tg.SomeChatId chatId
    , Tg.sendMessageText = text
    , Tg.sendMessageParseMode = parseMode
    , Tg.sendMessageDisableWebPagePreview = Nothing
    , Tg.sendMessageDisableNotification = Nothing
    , Tg.sendMessageReplyToMessageId = Nothing
    , Tg.sendMessageReplyMarkup = Nothing
    }
  where
    messageId = update & (Tg.updateMessage
                      >=> pure . Tg.messageMessageId)
    Just chatId = Tg.updateChatId update
