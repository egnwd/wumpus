module Wumpus.Movement
  ( getAction
  ) where

import Control.Monad.IO.Class
import Data.Char
import Data.Functor.Syntax
import Text.Read (readMaybe)

import Wumpus.Data
import qualified Wumpus.Messages as Msg

getAction :: (MonadIO m) => [Cave] -> m Action
getAction ts = liftIO $ getCommand <*> getCave ts

getCommand :: IO (Cave -> Action)
getCommand = do
  putStrLn $ Msg.moveOrShoot
  action <- toLower <$$> getLine
  case action of
    "m" -> return Move
    "s" -> return Shoot
    _   -> putStrLn Msg.moveOrShootInstr >> getCommand

getCave :: [Cave] -> IO Cave
getCave tunnels = do
  putStrLn $ Msg.whereTo
  cave <- readMaybe <$> getLine
  case cave of
    Just c | c `elem` tunnels -> return c
    _                         -> putStrLn (Msg.whereToInstr tunnels) >> getCave tunnels
