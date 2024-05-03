{-# LANGUAGE TypeFamilies #-}

module Interface.Blacklist where

import Control.Monad.Reader qualified as Transformer
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Database.Persist.Sql (SqlBackend)
import Effectful
import Effectful.Dispatch.Dynamic
import System.Timeout (timeout)
import Text.Regex.TDFA qualified as Regex

import Blacklist qualified
import Telegram.Bot.API qualified as Tg

type Blacklist = (Set Text, [Text])
class BlacklistItem item where
  blItem :: item -> (Text, Bool)
newtype BLRegex = BLRegex Text
instance BlacklistItem BLRegex where
  blItem (BLRegex txt) = (txt, True)
instance BlacklistItem Text where
  blItem txt = (txt, False)

data WithBlacklist :: Effect where
  GetBlacklist :: Tg.ChatId -> WithBlacklist m Blacklist
  AddBlacklistItem :: (BlacklistItem item) => Tg.ChatId -> item -> WithBlacklist m ()
  DelBlacklistItem :: (BlacklistItem item) => Tg.ChatId -> item -> WithBlacklist m ()
  CheckBlacklist :: Tg.ChatId -> Text -> WithBlacklist m Bool
type instance DispatchOf WithBlacklist = Dynamic

getBlacklist :: (HasCallStack, WithBlacklist :> es) => Tg.ChatId -> Eff es Blacklist
addBlacklistItem
  :: (HasCallStack, WithBlacklist :> es) => (BlacklistItem item) => Tg.ChatId -> item -> Eff es ()
delBlacklistItem
  :: (HasCallStack, WithBlacklist :> es) => (BlacklistItem item) => Tg.ChatId -> item -> Eff es ()
checkBlacklist :: (HasCallStack, WithBlacklist :> es) => Tg.ChatId -> Text -> Eff es Bool
getBlacklist chatId = send (GetBlacklist chatId)
addBlacklistItem chatId item = send (AddBlacklistItem chatId item)
delBlacklistItem chatId item = send (DelBlacklistItem chatId item)
checkBlacklist chatId txt = send (CheckBlacklist chatId txt)

runWithBlacklist
  :: (IOE :> es)
  => SqlBackend
  -> Eff (WithBlacklist : es) a
  -> Eff es a
runWithBlacklist conn = interpret $ \_ -> \case
  GetBlacklist chatId -> getBlacklist chatId
  AddBlacklistItem chatId item -> let (txt, isRegex) = blItem item in adapt $ Blacklist.add chatId txt isRegex
  DelBlacklistItem chatId item -> let (txt, isRegex) = blItem item in adapt $ Blacklist.del chatId txt isRegex
  CheckBlacklist chatId input -> do
    (plains, regexs0) <- getBlacklist chatId
    let regexs = Regex.makeRegex . T.unpack . (\x -> "^" <> x <> "$") <$> regexs0 :: [Regex.Regex]
    let docheck = pure $ T.drop 1 input `Set.member` plains || any (`Regex.matchTest` input) regexs
    fmap (fromMaybe True) $ liftIO $ timeout 1000000 docheck
  where
    getBlacklist chatId = do
      (plains, regexs) <- adapt $ Blacklist.get chatId
      pure (Set.fromList plains, regexs)
    adapt :: (IOE :> es) => Transformer.ReaderT SqlBackend IO a -> Eff es a
    adapt = liftIO . flip Transformer.runReaderT conn
