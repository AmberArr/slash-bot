module Config where

import qualified Telegram.Bot.API as Tg

newtype BotConfig = BotConfig
  { botCfgParseMode :: Maybe Tg.ParseMode
  }
