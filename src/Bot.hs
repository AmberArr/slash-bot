{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bot
       ( Env
       , BotM(..)
       , runBotM
       , runTgApi
       , runTgApi_
       ) where

import Control.Monad.Reader
import Data.Has
import qualified Data.Set as Set
import Servant.Client
import qualified Telegram.Bot.API as Tg

import Config
import Db
import Interface

type Env = (BotConfig, ClientEnv, DBConn)

newtype BotM a = BotM { unBotM :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance MonadBot BotM where
  getMe = runTgApi_ Tg.getMe
  sendMessage = runTgApi_ . Tg.sendMessage

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

runBotM :: BotM a -> Env -> IO a
runBotM (BotM bot) env = runReaderT bot env

runTgApi :: (Monad m, MonadReader r m, Has ClientEnv r, MonadIO m)
         => ClientM (Tg.Response a) -> m (Either ClientError a)
runTgApi client = do
  env <- asks getter
  liftIO $ runClientM (fmap Tg.responseResult client) env

runTgApi_ :: (Monad m, MonadReader r m, Has ClientEnv r, MonadIO m)
         => ClientM (Tg.Response a) -> m a
runTgApi_ client = do
  fmap (either (error . show) id) $ runTgApi client
