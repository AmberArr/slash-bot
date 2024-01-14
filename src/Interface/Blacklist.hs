module Interface.Blacklist where

import Data.Set (Set)
import Data.Text (Text)
import qualified Telegram.Bot.API as Tg

type Blacklist = (Set Text, [Text])
class BlacklistItem item where
  blItem :: item -> (Text, Bool)
newtype BLRegex = BLRegex Text
instance BlacklistItem BLRegex where
  blItem (BLRegex txt) = (txt, True)
instance BlacklistItem Text where
  blItem txt = (txt, False)

class WithBlacklist m where
  getBlacklist :: Tg.ChatId -> m Blacklist
  addBlacklistItem :: BlacklistItem item => Tg.ChatId -> item -> m ()
  delBlacklistItem :: BlacklistItem item => Tg.ChatId -> item -> m ()
  checkBlacklist :: Tg.ChatId -> Text -> m Bool
