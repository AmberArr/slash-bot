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

import Config
import Db
import Interface

type Env = (BotConfig, ClientEnv, DBConn)

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
  getBlacklist x = do
    conn <- asks getter
    liftIO $ Set.fromList <$> Db.getBlacklist conn x
  addBlacklistItem x y = do
    conn <- asks getter
    liftIO $ Db.addBlacklistItem conn x y
  delBlacklistItem x y = do
    conn <- asks getter
    liftIO $ Db.delBlacklistItem conn x y

runBotM :: BotM a -> Env -> IO (Either ClientError a)
runBotM (BotM bot) env@(_, clientEnv, _) =
  runClientM (runReaderT bot env) clientEnv

runBotM_ :: BotM a -> Env -> IO a
runBotM_ bot env =
  either (error . show) id <$> runBotM bot env

runTgApi :: (Monad m, MonadReader r m, Has ClientEnv r, MonadIO m)
         => ClientM (Tg.Response a) -> m (Either ClientError a)
runTgApi client = do
  env <- asks getter
  liftIO $ runClientM (fmap Tg.responseResult client) env

runTgApi_ :: (Monad m, MonadReader r m, Has ClientEnv r, MonadIO m)
         => ClientM (Tg.Response a) -> m a
runTgApi_ client = do
  either (error . show) id <$> runTgApi client
