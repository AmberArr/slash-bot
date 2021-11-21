{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
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
import System.IO.Unsafe
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.BotApp.Internal
import qualified Telegram.Bot.Simple.UpdateParser as P

data Action =
    NoOp
  | Inner !MessageId !ActionInner
  deriving (Show, Eq)

data ActionInner =
    Ping
  | BlackListAdd ![Text]
  | BlackListDel ![Text]
  | BlackListList
  | Reply !Text
  deriving (Show, Eq)

type Model = Set Text

-- Maybe I should put it in the `Model`, but anyway it works.
botName :: Text
botName =
  unsafePerformIO (maybe (error "Missing env: BOT_NAME") T.pack <$> lookupEnv "BOT_NAME")
{-# NOINLINE botName #-}

slashBot :: BotApp Model Action
slashBot = BotApp
  { botInitialModel = Set.empty
  , botAction = botAction'
  , botHandler = botHandler'
  , botJobs = []
  }

botAction' :: Update -> Model -> Maybe Action
botAction' update model = do
  messageId <- updateMessage update >>= pure . messageMessageId
  inner <- botActionInner update model
  pure $ Inner messageId inner

{-# ANN botAction' ("HLint: ignore Use <&>" :: String) #-}
-- Sorry for mixing jargons randomly.
botActionInner :: Update -> Model -> Maybe ActionInner
botActionInner update model = flip P.parseUpdate update $ do
  text <- P.text
  when (T.head text /= '/') $ fail "not a command"

  case T.words $ T.tail $ escape $ stripBotName text of
    ("_ping" : _)                      -> pure Ping
    ("_blacklist" : "add" : xs)        -> pure $ BlackListAdd xs
    ("_blacklist" : "del" : xs)        -> pure $ BlackListDel xs
    ["_blacklist"]                     -> pure BlackListList
    ((T.break (== '@') -> (cmd, botName)) : _)
      | cmd `Set.member` model || not (T.null botName)
                                       -> fail ""
    ["me"]                             -> fail ""
    ["you"]                            -> fail ""
    ("me" : (T.unwords -> predicate))  -> pure $ Reply [i|#{sender} #{predicate}！|]
    ("you" : (T.unwords -> predicate)) -> pure $ Reply [i|#{recipient} #{predicate}！|]
    [verb]                             -> pure $ Reply [i|#{sender} #{verb} 了 #{recipient}！|]
    (verb : (T.unwords -> patient))    -> pure $ Reply [i|#{sender} #{verb} #{recipient} #{patient}！|]
    []                                 -> fail ""
  where
    (sender0, senderId) = fromJust $
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

    messageId = fromJust $ messageMessageId <$> updateMessage update

    stripBotName text = case T.words text of
      ((T.stripSuffix ("@" <> botName) -> Just x) : xs) -> T.unwords (x:xs)
      _ -> text


botHandler' :: Action -> Model -> Eff Action Model
botHandler' NoOp model = pure model
botHandler' (Inner messageId actionInner) model = case actionInner of
  Ping -> model <# do
    reply' "111"
    pure NoOp
  BlackListAdd xs -> foldl' (flip Set.insert) model xs <# do
    reply' "ok"
    pure NoOp
  BlackListDel xs -> foldl' (flip Set.delete) model xs <# do
    reply' "ok"
    pure NoOp
  BlackListList -> model <# do
    let size = Set.size model
        s = if size == 1 then "" else "s" :: Text
    reply' $ T.unlines
      [ [i|Blacklisted #{size} command#{s}:|]
      , T.intercalate " " (Set.toList model)
      ]
    pure NoOp
  Reply x -> model <# do
    reply' x
    pure NoOp
  where
    reply' x =
      reply $ (toReplyMessage x){ replyMessageReplyToMessageId = Just messageId
                                , replyMessageParseMode = Just HTML
                                }

main :: IO ()
main = do
  token <- maybe (error "Missing env: TOKEN") (Token . T.pack) <$> lookupEnv "TOKEN"
  startBotWebhook (conversationBot updateChatId slashBot) token

startBotWebhook :: BotApp model action -> Token -> IO ()
startBotWebhook bot (Token token) = do
  env <- token `seq` defaultTelegramClientEnv (Token token)
  botenv <- startBotEnv bot env
  runEnv 3000 $ \req respond -> do
    if pathInfo req == ["bot" <> token]
      then do
        mupdate <- decode <$> strictRequestBody req
        forM_ mupdate $ \update -> void $ forkIO $ handleUpdate bot botenv update
        respond $ responseLBS status200 [] ""
      else do
        respond $ responseLBS status404 [] ""

startBotEnv :: BotApp model action -> ClientEnv -> IO (BotEnv model action)
startBotEnv bot env = do
  botEnv <- defaultBotEnv bot env
  _jobThreadIds <- scheduleBotJobs botEnv (botJobs bot)
  _actionsThreadId <- processActionsIndefinitely bot botEnv
  pure botEnv

handleUpdate :: BotApp model action -> BotEnv model action -> Update -> IO ()
handleUpdate BotApp{..} botEnv@BotEnv{..} update = do
  maction <- botAction update <$> readTVarIO botModelVar
  forM_ maction (issueAction botEnv (Just update))

mention :: UserId -> Text -> Text
mention (UserId userId) content =
  "<a href=\"tg://user?id=" <> T.pack (show userId) <> "\">" <> content <> "</a>"

escape :: Text -> Text
escape =
    T.replace "&" "&amp;"
  . T.replace "<" "&lt;"
  . T.replace ">" "&gt;"
