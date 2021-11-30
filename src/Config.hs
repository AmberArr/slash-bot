module Config where

import Data.Text (Text)
import qualified Telegram.Bot.API as Tg

data BotConfig = BotConfig
  { botCfgParseMode :: Maybe Tg.ParseMode
  , botCfgUsername :: Text
  }
