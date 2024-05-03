module Config where

import Telegram.Bot.API qualified as Tg

data BotConfig = BotConfig
  { botCfgParseMode :: Maybe Tg.ParseMode
  }
