module Util where

import Data.Text (Text)
import Data.Text qualified as T

escape :: Text -> Text
escape = T.concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  '&' -> "&amp;"
  c -> T.singleton c

unescape :: Text -> Text
unescape =
  T.replace "&amp;" "&"
    . T.replace "&lt;" "<"
    . T.replace "&gt;" ">"
    . T.replace "&quot;" "\""
    . T.replace "&#39;" "'"

codeMarkup :: Text -> Text
codeMarkup txt = "<code>" <> escape txt <> "</code>"
