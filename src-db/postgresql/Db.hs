module Db
  ( withDBConn
  ) where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Sql (SqlBackend)
import UnliftIO (MonadUnliftIO)

import Database.Persist.Postgresql (withPostgresqlConn)

withDBConn
  :: (MonadUnliftIO m, MonadLoggerIO m)
  => Text -> (SqlBackend -> m a) -> m a
withDBConn connstr = withPostgresqlConn (encodeUtf8 connstr)

