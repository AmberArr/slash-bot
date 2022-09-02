{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Parser where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Function
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
  , sender :: Text
  , recipient :: Text
  , remainder :: [Text]
  , isActiveVoice :: Bool
  } deriving (Show, Eq)

parseUpdate :: (MonadError () m, MonadIO m) => Tg.Update -> Text -> m CmdInfo
parseUpdate update botUsername = do
  text <- liftMaybe $ P.runUpdateParser P.text update
  isActiveVoice <- case T.head text of
    '/' -> pure True
    '\\' -> pure False
    _ -> throwError ()

  let words = T.words $ T.tail $ escape text
  ((cmd, username0), remainder0) <- case words of
    [] -> throwError ()
    (head : tail) -> pure (splitAtAt head, tail)

  -- It's kinda dumb. But whatever, it works.
  let checkSecondWord = \case
        (secondWord : rem) | "@" `T.isPrefixOf` secondWord ->
          let username = T.tail secondWord
           in (\x -> (Just x, username, rem)) <$> fetchFirstName username
        _ ->
          pure (Nothing, username0, remainder0)

  (maybeRecipient0, username, remainder) <- if
    | T.null username0 ->
        checkSecondWord remainder0
    | username0 == botUsername ->
        checkSecondWord remainder0
    | "bot" `T.isSuffixOf` T.toLower username0 ->
        throwError ()
    | otherwise ->
        (\x -> (Just x, username0, remainder0)) <$> fetchFirstName username0

  liftMaybe $ do
    (senderName, senderLinkBuilder) <-
      update & (Tg.updateMessage >=> getSenderFromMessage)

    let sender = maybe "Ta" senderLinkBuilder senderName
        f = \x -> (Just x, mentionWithUsername username)
        maybeRecipient = msum
          [ fmap f maybeRecipient0
          , update & (Tg.updateMessage
                  >=> Tg.messageReplyToMessage
                  >=> getSenderFromMessage)
          ]
        recipient = case maybeRecipient of
          Just (Just recipientName, recipientLinkBuilder) ->
            recipientLinkBuilder recipientName
          _ ->
            senderLinkBuilder "自己"
    pure CmdInfo{..}

fetchFirstName :: MonadIO m => Text -> m Text
fetchFirstName username = do
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
