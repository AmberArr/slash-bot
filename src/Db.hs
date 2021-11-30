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
import Data.String.Interpolate (iii)
import Database.PostgreSQL.Simple

type DBConn = Connection

mkTable :: Connection -> IO ()
mkTable conn = () <$ execute_ conn [iii|CREATE TABLE IF NOT EXISTS blacklist_command (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  chat_id INTEGER,
  command TEXT
)|]

withDBConn :: BS.ByteString -> (Connection -> IO ()) -> IO ()
withDBConn dburl go =
  bracket
    (connectPostgreSQL dburl)
    close
    go
