{-# LANGUAGE QuasiQuotes #-}
module Db.Blacklist where

import Data.String.Interpolate (iii)
import Data.Text (Text)
import Database.SQLite.Simple
import qualified Telegram.Bot.API as Tg

addBlacklistItem :: Connection -> Tg.ChatId -> Text -> IO ()
addBlacklistItem conn (Tg.ChatId chatId) cmd = execute conn [iii|
  INSERT INTO blacklist_command (chat_id, command)
  VALUES (?, ?) ON CONFLICT DO NOTHING
|] (chatId, cmd)

delBlacklistItem :: Connection -> Tg.ChatId -> Text -> IO ()
delBlacklistItem conn (Tg.ChatId chatId) cmd = execute conn [iii|
  DELETE FROM blacklist_command
  WHERE chat_id = ? AND command = ?
|] (chatId, cmd)

getBlacklist :: Connection -> Tg.ChatId -> IO [Text]
getBlacklist conn (Tg.ChatId chatId) = fmap fromOnly <$> query conn [iii|
  SELECT command
  FROM blacklist_command
  WHERE chat_id = ?
|] (Only chatId)
