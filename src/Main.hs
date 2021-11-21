{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Monad
import Data.Aeson (decode)
import Data.Function
import Data.List (foldl')
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Conc
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (runEnv)
import Servant.Client
import System.Environment
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.BotApp.Internal
import qualified Telegram.Bot.Simple.UpdateParser as P

data Action =
    NoOp
  | Ping
  | BlackListAdd ![Text]
  | BlackListDel ![Text]
  | Reply !Text
  deriving (Read, Show, Eq, Ord)

type Model = Set Text

slashBot :: BotApp Model Action
slashBot = BotApp
  { botInitialModel = Set.empty
  , botAction = botAction'
  , botHandler = botHandler'
  , botJobs = []
  }

{-# ANN botAction' ("HLint: ignore Use <&>" :: String) #-}
-- sorry for mixing jargons randomly
botAction' :: Update -> Model -> Maybe Action
botAction' update model = flip P.parseUpdate update $ do
  text <- P.text
  when (T.head text /= '/') $ fail "not a command"

  let (sender0, senderId) = fromJust $
        update & (updateMessage
              >=> messageFrom
              >=> pure . (userFirstName &&& userId))
      sender = mention senderId sender0

      (recipient0, recipientId) = fromMaybe ("自己", senderId) $
        update & (updateMessage
              >=> messageReplyToMessage
              >=> messageFrom
              >=> pure . (userFirstName &&& userId))
      recipient = mention recipientId recipient0

  case T.words $ T.tail $ escape text of
    ("/ping" : _)                  -> pure Ping
    ("/blacklist" : "add" : xs)    -> pure $ BlackListAdd xs
    ("/blacklist" : "del" : xs)    -> pure $ BlackListDel xs
    (x : _) | x `Set.member` model -> fail ""
    ["me"]                         -> fail ""
    ["you"]                        -> fail ""
    ("me" : predicate)             -> pure $ Reply [i|#{sender} #{predicate}|]
    ("you" : predicate)            -> pure $ Reply [i|#{recipient} #{predicate}|]
    [verb]                         -> pure $ Reply [i|#{sender} #{verb} 了 #{recipient}|]
    (verb : patient)               -> pure $ Reply [i|#{sender} #{verb} #{recipient} #{patient}|]
    []                             -> fail ""


botHandler' :: Action -> Model -> Eff Action Model
botHandler' action model = case action of
  NoOp -> pure model
  Ping -> model <# do
    replyText "111"
    return NoOp
  BlackListAdd xs -> foldl' (flip Set.insert) model xs <# do
    replyText "ok"
    return NoOp
  BlackListDel xs -> foldl' (flip Set.delete) model xs <# do
    replyText "ok"
    return NoOp
  Reply x -> model <# do
    reply $ (toReplyMessage x){replyMessageParseMode = Just HTML}
    return NoOp

main :: IO ()
main = do
  token <- maybe (error "Missing env: TOKEN") (Token . T.pack) <$> lookupEnv "TOKEN"
  env <- token `seq` defaultTelegramClientEnv token
  startBotWebhook (conversationBot updateChatId slashBot) env

startBotWebhook :: BotApp model action -> ClientEnv -> IO ()
startBotWebhook bot env = do
  botenv <- startBotEnv bot env
  runEnv 3000 $ \req respond -> do
    update <- fromJust . decode <$> strictRequestBody req
    void $ forkIO $ handleUpdate bot botenv update
    respond $ responseLBS status200 [] ""

startBotEnv :: BotApp model action -> ClientEnv -> IO (BotEnv model action)
startBotEnv bot env = do
  botEnv <- defaultBotEnv bot env
  _jobThreadIds <- scheduleBotJobs botEnv (botJobs bot)
  _actionsThreadId <- processActionsIndefinitely bot botEnv
  return botEnv

handleUpdate :: BotApp model action -> BotEnv model action -> Update -> IO ()
handleUpdate BotApp{..} botEnv@BotEnv{..} update = do
  maction <- botAction update <$> readTVarIO botModelVar
  forM_ maction (issueAction botEnv (Just update))

mention :: UserId -> Text -> Text
mention userId content =
  "<a href=\"tg://user?id=" <> T.pack (show userId) <> "\">" <> content <> "</a>"

escape :: Text -> Text
escape =
    T.replace "&" "&amp;"
  . T.replace "<" "&lt;"
  . T.replace ">" "&gt;"
