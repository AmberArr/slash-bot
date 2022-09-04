{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Parser
  ( CmdInfo(..)
  , parseUpdate
  ) where

import Control.Lens ((^.), (&), (...), cosmos)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Function (on)
import Data.List (sortOn)
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Text (Text)
import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest_)
import Text.XML.Lens (attributeIs, named, root, text)
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
  , isIgnoreBlacklist :: Bool
  } deriving (Show, Eq)

-- sorry for my s**t code
parseUpdate :: (MonadError () m, MonadIO m) => Tg.Update -> Text -> m CmdInfo
parseUpdate update botUsername = do
  text <- liftMaybe $ P.runUpdateParser P.text update
  isActiveVoice <- case T.head text of
    '/' -> pure True
    '\\' -> pure False
    _ -> throwError ()

  frags <- pure $ breakText update $ T.tail $ escape text
  (frags, isIgnoreBlacklist) <-
    flip runStateT False $ handleTargetUsername botUsername frags
  let (isAltSubject, isAltRecipient) = case frags of
        (x: _)    | isMention x -> (True, False)
        (_: y: _) | isMention y -> (False, True)
        _ -> (False, False)
  words <- traverse tryConvertMentionToLink frags
  (cmd, altSubject, altRecipient, remainder) <-
    case (words, isAltSubject, isAltRecipient) of
      (  x:y, True,    _) -> pure ("",  Just x, Nothing, y)
      (x:y:z,    _, True) -> pure ( x, Nothing,  Just y, z)
      (  x:y,    _,    _) -> pure ( x, Nothing, Nothing, y)
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

data TextFragment =
    PlainText !Text
  | Mention !Text
  | TextMention !Tg.UserId !Text
  deriving (Eq, Show)

isMention :: TextFragment -> Bool
isMention = \case
  PlainText _     -> False
  Mention _       -> True
  TextMention _ _ -> True

breakText :: Tg.Update -> Text -> [TextFragment]
breakText update text = do
  let frags = extractTextMention update text
  flip concatMap frags $ \case
    PlainText x -> extractMention <$> T.words x
    x -> [x]

extractMention :: Text -> TextFragment
extractMention x =
  if "@" `T.isPrefixOf` x
     then Mention (T.tail x)
     else PlainText x

getTextMentionEntities :: Tg.Update -> [Tg.MessageEntity]
getTextMentionEntities update =
  case Tg.messageEntities =<< Tg.extractUpdateMessage update of
    Nothing -> []
    Just [] -> []
    Just xs ->
      flip filter xs $ \entity ->
        Tg.messageEntityType entity == Tg.MessageEntityTextMention

extractTextMention :: Tg.Update -> Text -> [TextFragment]
extractTextMention update text =
  case Tg.messageEntities =<< Tg.extractUpdateMessage update of
    Nothing -> [PlainText text]
    Just [] -> [PlainText text]
    Just xs ->
      let entities = flip filter xs $ \entity ->
            Tg.messageEntityType entity == Tg.MessageEntityTextMention
          sortOnOffset = sortOn Tg.messageEntityOffset
       in replacing entities 1 text -- 1 since the initial slash is striped
  where
    replacing :: [Tg.MessageEntity] -> Int -> Text -> [TextFragment]
    replacing [] _idx text = [PlainText text]
    replacing (Tg.MessageEntity{..} : xs) idx text = do
      let offset = messageEntityOffset
          length = messageEntityLength
          Tg.User{..} = fromJust messageEntityUser
          (a, b) = T.splitAt (offset - idx) text
          (c, d) = T.splitAt length b

       in ( PlainText a
          : TextMention userId c
          : replacing xs (idx + offset + length) d
          )

handleTargetUsername ::
  ( MonadError () m
  , MonadState Bool m -- isIgnoreBlacklist
  )
  => Text
  -> [TextFragment]
  -> m [TextFragment]
handleTargetUsername botUsername (PlainText x : remainder) = do
  let (cmd, username) = splitAtAt x
  if
    | T.null cmd && T.null username ->
        throwError ()
    | T.null cmd ->
        pure (Mention username : remainder)
    | T.null username ->
        pure (PlainText cmd : remainder)
    | username == botUsername ->
        put True >> pure (PlainText cmd : remainder)
    | "bot" `T.isSuffixOf` T.toLower username ->
        throwError ()
    | otherwise ->
        pure (PlainText cmd : Mention username : remainder)
handleTargetUsername _ [] = throwError ()
handleTargetUsername _ x = pure x

tryConvertMentionToLink :: MonadIO m => TextFragment -> m Text
tryConvertMentionToLink = \case
  PlainText x -> pure x
  TextMention userId name -> pure $ mentionWithId userId name
  Mention username -> do
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