{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Bot
       ( Env
       , BotT(..)
       , BotM(..)
       , runBotM
       , runBotM_
       , runTgApi
       , runTgApi_
       ) where

import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Has
import qualified Data.Set as Set
import Servant.Client
import qualified Telegram.Bot.API as Tg
import Database.Persist.Sql (SqlBackend)

import Config
import Interface
import qualified Blacklist

type Env = (BotConfig, ClientEnv, SqlBackend)

newtype BotT m a = BotM { unBotM :: ReaderT Env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

type BotM = BotT ClientM

instance MonadTrans BotT where
  lift = BotM . lift

instance MonadBot Env BotM where
  getMe = lift $ fmap Tg.responseResult Tg.getMe
  sendMessage req = lift $ fmap Tg.responseResult (Tg.sendMessage req)

instance MonadTransControl BotT where
  type StT BotT a = a
  liftWith = defaultLiftWith BotM unBotM
  restoreT = defaultRestoreT BotM

instance WithBlacklist BotM where
  getBlacklist x       = Set.fromList <$> Blacklist.get x
  addBlacklistItem x y = Blacklist.add x y
  delBlacklistItem x y = Blacklist.del x y

runBotM :: BotM a -> Env -> IO (Either ClientError a)
runBotM (BotM bot) env@(_, clientEnv, _) =
  runClientM (runReaderT bot env) clientEnv

runBotM_ :: BotM a -> Env -> IO ()
runBotM_ bot env = do
  result <- runBotM bot env
  case result of
    Left e -> print e
    _ -> pure ()

runTgApi :: (Monad m, MonadReader r m, Has ClientEnv r, MonadIO m)
         => ClientM (Tg.Response a) -> m (Either ClientError a)
runTgApi client = do
  env <- asks getter
  liftIO $ runClientM (fmap Tg.responseResult client) env

runTgApi_ :: (Monad m, MonadReader r m, Has ClientEnv r, MonadIO m)
         => ClientM (Tg.Response a) -> m a
runTgApi_ client = do
  either (error . show) id <$> runTgApi client
