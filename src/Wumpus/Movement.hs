module Wumpus.Movement
  ( getAction
  ) where

import Control.Monad.IO.Class
import Data.Char
import Data.Functor.Syntax
import Text.Read (readMaybe)
import Prelude hiding (putStrLn)

import Wumpus.Data
import qualified Wumpus.Messages as Msg
import Wumpus.Utils (putStrLn)

getAction :: (MonadIO m) => [Cave] -> m Action
getAction ts = getCommand <*> getCave ts

getCommand :: (MonadIO m) => m (Cave -> Action)
getCommand = do
  putStrLn Msg.moveOrShoot
  action <- toLower <$$> liftIO getLine
  case action of
    "m" -> return Move
    "s" -> return Shoot
    _   -> putStrLn Msg.moveOrShootInstr >> getCommand

getCave :: (MonadIO m) => [Cave] -> m Cave
getCave tunnels = do
  putStrLn Msg.whereTo
  cave <- readMaybe <$> liftIO getLine
  case cave of
    Just c | c `elem` tunnels -> return c
    _                         -> putStrLn (Msg.whereToInstr tunnels) >> getCave tunnels
