{-# LANGUAGE QuasiQuotes #-}
module Db
       ( module Db.Blacklist
       , mkTable
       , DBConn
       ) where

import Db.Blacklist

import Data.String.Interpolate (iii)
import Database.SQLite.Simple

type DBConn = Connection

mkTable :: Connection -> IO ()
mkTable conn = execute_ conn [iii|CREATE TABLE IF NOT EXISTS blacklist_command (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  chat_id INTEGER,
  command TEXT
)|]
