module Db
  ( withDBConn
  ) where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Sql (SqlBackend)
import UnliftIO (MonadUnliftIO)

import Database.Persist.Sqlite (withSqliteConn)

withDBConn
  :: (MonadUnliftIO m, MonadLoggerIO m)
  => Text -> (SqlBackend -> m a) -> m a
withDBConn = withSqliteConn
