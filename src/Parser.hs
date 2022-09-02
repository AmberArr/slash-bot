{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Parser where
module Parser
  ( CmdInfo(..)
  , parseUpdate
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Function
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Text (Text)
import Network.HTTP.Simple
import Text.XML.Lens
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Telegram.Bot.API as Tg
import qualified Telegram.Bot.Simple.UpdateParser as P
import qualified Text.HTML.DOM as HTML

data CmdInfo = CmdInfo
  { cmd :: Text
  , subject :: Text
  , recipient :: Text
  , remainder :: [Text]
  , isActiveVoice :: Bool
  , isAltSubject :: Bool
  } deriving (Show, Eq)

-- sorry for my s**t code
parseUpdate :: (MonadError () m, MonadIO m) => Tg.Update -> Text -> m CmdInfo
parseUpdate update botUsername = do
  text <- liftMaybe $ P.runUpdateParser P.text update
  isActiveVoice <- case T.head text of
    '/' -> pure True
    '\\' -> pure False
    _ -> throwError ()

  let words0 = T.words $ T.tail $ escape text
  words1 <- handleTargetUsername botUsername words0

  let (isAltSubject, isAltRecipient) = case words1 of
        (firstWord : _)      | "@" `T.isPrefixOf` firstWord  -> (True, False)
        (_ : secondWord : _) | "@" `T.isPrefixOf` secondWord -> (False, True)
        _ -> (False, False)
  words <- traverse tryConvertMentionToLink words1
  (cmd, altSubject, altRecipient, remainder) <-
    case (words, isAltSubject, isAltRecipient) of
      (x:y,   True, _)    -> pure ("", Just x,  Nothing, y)
      (x:y:z, _,    True) -> pure (x,  Nothing, Just y,  z)
      (x:y,   _,    _)    -> pure (x,  Nothing, Nothing, y)
      _ -> throwError ()

  liftMaybe $ do
    (subjectName, subjectLinkBuilder) <-
      update & (Tg.updateMessage >=> getSenderFromMessage)

    let subject = maybe "Ta" subjectLinkBuilder $ msum
          [ altSubject
          , subjectName
          ]
        maybeRecipient =
          update & (Tg.updateMessage
                >=> Tg.messageReplyToMessage
                >=> getSenderFromMessage)
        recipient = case maybeRecipient of
          Just (Just recipientName, recipientLinkBuilder) ->
            recipientLinkBuilder recipientName
          _ -> case (isAltSubject, altRecipient) of
                  (True, _)   -> T.empty
                  (_, Just x) -> x
                  _ -> subjectLinkBuilder "自己"
    pure CmdInfo{..}

handleTargetUsername :: MonadError () m => Text -> [Text] -> m [Text]
handleTargetUsername botUsername words = do
  ((cmd, username), remainder) <- case words of
    (head : tail) -> pure (splitAtAt head, tail)
    [] -> throwError ()
  let atUsername = T.cons '@' username
  if
    | T.null username || username == botUsername ->
        pure (cmd : remainder)
    | "bot" `T.isSuffixOf` T.toLower username ->
        throwError ()
    | T.null cmd ->
        pure (atUsername : remainder)
    | otherwise ->
        pure (cmd : atUsername : remainder)

tryConvertMentionToLink :: MonadIO m => Text -> m Text
tryConvertMentionToLink word =
  if "@" `T.isPrefixOf` word
     then fetch (T.tail word)
     else pure word
  where
    fetch username = do
      name <- fetchName username
      pure $ mentionWithUsername username name

fetchName :: MonadIO m => Text -> m Text
fetchName username = do
  let req = parseRequest_ $ T.unpack $ toTgUserWebLink username
  resp <- liftIO $ httpBS req
  pure $ extractNameFromHTML $ T.decodeUtf8 $ getResponseBody resp

extractNameFromHTML :: Text -> Text
extractNameFromHTML htmlText = html ^. lens
  where
    html = HTML.parseLT $ LT.fromStrict htmlText
    lens =
      root
      . cosmos
      . attributeIs "class" "tgme_page_title"
      ... named "span"
      . text

-- return sender name and mention link builder (if it can)
getSenderFromMessage :: Tg.Message -> Maybe (Maybe Text, Text -> Text)
getSenderFromMessage msg = do
  from <- Tg.messageFrom msg
  Tg.UserId userId <- pure $ Tg.userId from
  case userId of
    -- 136817688: if sender is "@Channel_Bot"
    -- 1087968824: if sender is "@GroupAnonymousBot"
    x | x == 136817688 || x == 1087968824 -> do
      senderChat <- Tg.messageSenderChat msg
      let name = Tg.chatTitle senderChat
      username <- Tg.chatUsername senderChat
      pure (name, mentionWithUsername username)
    realUserId -> do
      let name = Tg.userFirstName from
      pure (Just name, mentionWithId (Tg.UserId realUserId))

mentionWithId :: Tg.UserId -> Text -> Text
mentionWithId (Tg.UserId userId) content =
  [i|<a href="tg://user?id=#{userId}">#{content}</a>|]

mentionWithUsername :: Text -> Text -> Text
mentionWithUsername username content =
  [i|<a href="#{toTgUserLink username}">#{content}</a>|]

toTgUserLink :: Text -> Text
toTgUserLink username = "tg://resolve?domain=" <> username

toTgUserWebLink :: Text -> Text
toTgUserWebLink username = "https://t.me/" <> username

liftMaybe :: MonadError () m => Maybe a -> m a
liftMaybe = maybe (throwError ()) pure

splitAtAt :: Text -> (Text, Text)
splitAtAt text =
  case T.break (== '@') text of
    (x, y) | "@" `T.isPrefixOf` y -> (x, T.tail y)
    x -> x

escape :: Text -> Text
escape =
    T.replace "&" "&amp;"
  . T.replace "<" "&lt;"
  . T.replace ">" "&gt;"
  . T.replace "\"" "&quot;"
  . T.replace "'"  "&#39;"

unescape :: Text -> Text
unescape =
    T.replace "&amp;"   "&"
  . T.replace "&lt;"    "<"
  . T.replace "&gt;"    ">"
  . T.replace "&quot;"  "\""
  . T.replace "&#39;"   "'"
