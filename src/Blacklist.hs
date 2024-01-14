{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Blacklist where

import Control.Monad.Reader
import Data.Either
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
    isRegex Bool default=False
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
  -> Bool
  -> m ()
add (Tg.ChatId i) cmd isRegex = toClassy $ void $
  insertRecord (Blacklist (fromInteger i) cmd isRegex)

del
  :: ( MonadIO m
     , MonadReader r m
     , Has SqlBackend r
     )
  => Tg.ChatId
  -> Text
  -> Bool
  -> m ()
del (Tg.ChatId i) cmd isRegex = toClassy $ void $
  deleteWhere
    [ BlacklistChatId ==. fromInteger i
    , BlacklistCommand ==. cmd
    , BlacklistIsRegex ==. isRegex
    ]

get
  :: ( MonadIO m
     , MonadReader r m
     , Has SqlBackend r
     )
  => Tg.ChatId
  -> m ([Text], [Text])
get (Tg.ChatId i) = toClassy $ do
  results <- selectList [ BlacklistChatId ==. fromInteger i ] []
  pure $ partitionEithers (fmap f results)
    where
      f entity =
        let val = entityVal entity
            txt = blacklistCommand val
            isRegex = blacklistIsRegex val
        in if isRegex then Right txt else Left txt
