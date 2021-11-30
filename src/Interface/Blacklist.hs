module Interface.Blacklist where

import Data.Set (Set)
import Data.Text (Text)
import qualified Telegram.Bot.API as Tg

type Blacklist = Set Text

class WithBlacklist m where
  getBlacklist :: Tg.ChatId -> m Blacklist
  addBlacklistItem :: Tg.ChatId -> Text -> m ()
  delBlacklistItem :: Tg.ChatId -> Text -> m ()
