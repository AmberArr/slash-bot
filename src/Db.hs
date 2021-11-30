{-# LANGUAGE QuasiQuotes #-}
module Db
       ( module Db.Blacklist
       , mkTable
       , withDBConn
       , DBConn
       ) where

import Db.Blacklist

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

type DBConn = Connection

mkTable :: Connection -> IO ()
mkTable conn = () <$ execute_ conn [sql|CREATE TABLE IF NOT EXISTS blacklist_command (
  id SERIAL PRIMARY KEY,
  chat_id BIGINT,
  command TEXT
)|]

withDBConn :: BS.ByteString -> (Connection -> IO ()) -> IO ()
withDBConn dburl go =
  bracket
    (connectPostgreSQL dburl)
    close
    go
