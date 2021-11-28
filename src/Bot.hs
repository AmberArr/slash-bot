{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bot where

import Control.Monad.Reader
import Data.Has
import Servant.Client
import qualified Telegram.Bot.API as Tg

import Config
import Interface.Bot

type Env = (BotConfig, ClientEnv)

mkEnv :: BotConfig -> ClientEnv -> Env
mkEnv x y = (x,y)

newtype BotM a = BotM { unBotM :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runBotM :: BotM a -> Env -> IO a
runBotM (BotM bot) env = runReaderT bot env

instance MonadBot BotM where
  getMe = runTgApi_ Tg.getMe
  sendMessage = runTgApi_ . Tg.sendMessage

runTgApi :: (Monad m, MonadReader r m, Has ClientEnv r, MonadIO m)
         => ClientM (Tg.Response a) -> m (Either ClientError a)
runTgApi client = do
  env <- asks getter
  liftIO $ runClientM (fmap Tg.responseResult client) env

runTgApi_ :: (Monad m, MonadReader r m, Has ClientEnv r, MonadIO m)
         => ClientM (Tg.Response a) -> m a
runTgApi_ client = do
  fmap (either (error . show) id) $ runTgApi client
