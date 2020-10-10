-- vim: fdm=marker
{-# LANGUAGE FlexibleContexts, MultiWayIf, ConstraintKinds, LambdaCase #-}
module Wumpus
  ( runWumpus

  , WorldConfig(..)
  , maze
  , pits
  , bats
  , isDebug
  , hazards
  , initialWorld

  , GameState(..)
  , crookedArrows
  , gCave
  , gHistory
  , trevor
  , gameOver
  , initialState
  ) where

-- {{{
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Random.Class
import Control.Lens
import Control.Conditional
import Data.Foldable
import Data.Graph
import Data.List
import Data.Maybe.HT
import Prelude hiding (putStrLn)

import Wumpus.Data
import Wumpus.Movement
import Wumpus.Utils
import qualified Wumpus.Messages as Msg
-- }}}

runWumpus :: (MonadIO m, MonadRandom m) => GameState -> WorldConfig -> m GameState
runWumpus = runReaderT . execStateT loop

data MoveEvent  = Wumpus | Bat | Pit
data ShootEvent = Kill | OutOfAmmo

type Game m = (MonadState GameState m, MonadReader WorldConfig m, MonadRandom m)
type Logging = WriterT [String]

loop :: (Game m, MonadIO m) => m ()
loop = do
-- {{{
  -- Cave info
  use gCave >>= putStrLn . Msg.youAreInCave
  execWriterT sense >>= traverse_ putStrLn

  tunnels <- sort <$> view maze <!> use gCave
  putStrLn (Msg.tunnelsLeadTo tunnels)
  action <- getAction tunnels

  -- Player action
  d <- view isDebug
  let shouldDebug l = d || (not $ isPrefixOf "[DEBUG]" l)
  logs <- execWriterT $ censor (filter shouldDebug) (execute action)
  traverse_ putStrLn logs

  use gameOver >>= \case
      Nothing -> loop
      Just Win -> putStrLn Msg.win
      Just Lose -> putStrLn Msg.lose
-- }}}

sense :: (Game m) => Logging m ()
sense = do
-- {{{
  let tunnels = view maze <!> use gCave
  whenM (elem <$> use trevor <*> tunnels)   $ logMsg Msg.senseWumpus
  whenM (anyM (isIn <$> view bats) tunnels) $ logMsg Msg.senseBats
  whenM (anyM (isIn <$> view pits) tunnels) $ logMsg Msg.sensePits
-- }}}

execute :: (Game m) => Action -> Logging m ()

execute a@(Move c) = do
  gCave .= c >> (logDebug $ "Updated history with action " ++ show a)
  updateHistory a >> (logDebug $ "Updated cave to Cave " ++ show c)

  getMoveEvent c >>= \case
    Just Wumpus -> (gameOver ?= Lose) >> logMsg Msg.encounterWumpus
    Just Pit    -> (gameOver ?= Lose) >> logMsg Msg.losePits
    Just Bat    -> do
      gCave .=<< emptyCave
      logDebug $ "Updated history with action " ++ show a
      logMsg Msg.encounterBats
    Nothing -> return ()

execute a@(Shoot c) = do
-- {{{
  updateHistory a >> (logDebug $ "Updated cave to Cave " ++ show c)
  crookedArrows -= 1
  getShootEvent c >>= \case
    Just Kill      -> (gameOver ?= Win)  >> logMsg  Msg.winWumpus
    Just OutOfAmmo -> (gameOver ?= Lose) >> logMsgs [Msg.missed, Msg.loseArrows]
    Nothing -> do
      trevor' <- trevor <.=<< anotherCave

      if   trevor' == c
      then (gameOver ?= Lose) >> logMsg Msg.loseWumpus
      else logMsg Msg.missed
-- }}}

updateHistory :: (MonadState GameState m) => Action -> m ()
updateHistory a = gHistory %= (a :)

emptyCave :: (Game m) => m Cave
emptyCave = do
-- {{{
  hs <- use trevor <:> view hazards
  cs <- filter (not . isIn hs) . vertices <$> view maze
  randomCave cs
-- }}}

anotherCave :: (Game m) => m Cave
anotherCave = do
-- {{{
  t <- use trevor
  cs <- filter (not . (t ==)) . vertices <$> view maze

  randomCave cs
-- }}}

randomCave :: (MonadRandom m) => [Cave] -> m Cave
randomCave cs = do
-- {{{
  c <- getRandomR (1, length cs)
  return $ cs !! (c-1)
-- }}}

getMoveEvent :: (Game m) => Cave -> m (Maybe MoveEvent)
getMoveEvent c = do
-- {{{
  isTrevorHere <- (c ==) <$> use trevor
  areBatsHere  <- (c `elem`) <$> view bats
  arePitsHere  <- (c `elem`) <$> view pits

  return $ (toMaybe isTrevorHere Wumpus)
    <|> (toMaybe areBatsHere Bat)
    <|> (toMaybe arePitsHere Pit)
-- }}}

getShootEvent :: (Game m) => Cave -> m (Maybe ShootEvent)
getShootEvent c = do
-- {{{
  isTrevorHere <- (c ==) <$> use trevor
  isOutOfAmmo  <- (0 ==) <$> use crookedArrows

  return $ toMaybe isTrevorHere Kill <|> toMaybe isOutOfAmmo OutOfAmmo
-- }}}

