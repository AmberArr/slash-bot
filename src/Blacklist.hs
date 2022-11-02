{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Blacklist where

import Control.Monad.Reader
import Data.Has
import Data.Int
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import qualified Telegram.Bot.API as Tg

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Blacklist
    chatId Int64
    command Text
    deriving Show
|]

toClassy
  :: ( MonadIO m
     , MonadReader r m
     , Has SqlBackend r
     )
  => ReaderT SqlBackend IO a
  -> m a
toClassy reader = do
  sqlBackend <- asks getter
  liftIO $ runReaderT reader sqlBackend

add
  :: ( MonadIO m
     , MonadReader r m
     , Has SqlBackend r
     )
  => Tg.ChatId
  -> Text
  -> m ()
add (Tg.ChatId i) cmd = toClassy $ void $
  insertRecord (Blacklist (fromInteger i) cmd)

del
  :: ( MonadIO m
     , MonadReader r m
     , Has SqlBackend r
     )
  => Tg.ChatId
  -> Text
  -> m ()
del (Tg.ChatId i) cmd = toClassy $ void $
  deleteWhere
    [ BlacklistChatId ==. fromInteger i
    , BlacklistCommand ==. cmd
    ]

get
  :: ( MonadIO m
     , MonadReader r m
     , Has SqlBackend r
     )
  => Tg.ChatId
  -> m [Text]
get (Tg.ChatId i) = toClassy $ do
  results <- selectList [ BlacklistChatId ==. fromInteger i ] []
  pure $ fmap (blacklistCommand . entityVal) results
