{-# LANGUAGE QuasiQuotes #-}
module Db.Blacklist where

import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import qualified Telegram.Bot.API as Tg

addBlacklistItem :: Connection -> Tg.ChatId -> Text -> IO ()
addBlacklistItem conn (Tg.ChatId chatId) cmd = () <$ execute conn [sql|
  INSERT INTO blacklist_command (chat_id, command)
  VALUES (?, ?) ON CONFLICT DO NOTHING
|] (chatId, cmd)

delBlacklistItem :: Connection -> Tg.ChatId -> Text -> IO ()
delBlacklistItem conn (Tg.ChatId chatId) cmd = () <$ execute conn [sql|
  DELETE FROM blacklist_command
  WHERE chat_id = ? AND command = ?
|] (chatId, cmd)

getBlacklist :: Connection -> Tg.ChatId -> IO [Text]
getBlacklist conn (Tg.ChatId chatId) = fmap fromOnly <$> query conn [sql|
  SELECT command
  FROM blacklist_command
  WHERE chat_id = ?
|] (Only chatId)
