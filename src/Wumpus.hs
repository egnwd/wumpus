-- vim: fdm=marker
{-# LANGUAGE FlexibleContexts, MultiWayIf, ConstraintKinds, LambdaCase #-}
module Wumpus
  ( runWumpus
  , GameState(..)
  , WorldConfig(..)
  , initialState
  , initialWorld
  ) where

-- {{{
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Data.Graph
import Data.List
import Prelude hiding (putStrLn)
import System.Random
import Data.Maybe.HT
import Data.Foldable
import Util

import Wumpus.Data
import Wumpus.Movement
import qualified Wumpus.Messages as Msg
import Wumpus.Utils
-- }}}

runWumpus :: MonadIO m => GameState -> WorldConfig -> m GameState
runWumpus = runReaderT . execStateT loop

data MoveEvent  = Wumpus | Bat | Pit
data ShootEvent = Kill | OutOfAmmo

type Game m = (MonadState GameState m, MonadReader WorldConfig m)

loop :: (Game m, MonadIO m) => m ()
loop = do
-- {{{
  m <- asks maze
  cave <- gets gCave
  let tunnels = sort (m!cave)

  -- Cave info
  putStrLn $ Msg.youAreInCave cave
  sense >>= traverse_ putStrLn
  putStrLn (Msg.tunnelsLeadTo tunnels)
  action <- getAction tunnels

  -- Player action
  d <- asks isDebug
  let shouldDebug l = d || (not $ isPrefixOf "[DEBUG]" l)
  logs <- execute action
  traverse_ putStrLn $ filter shouldDebug logs

  gets gameOver >>= \case
      Nothing -> loop
      Just Win -> putStrLn Msg.win
      Just Lose -> putStrLn Msg.lose
-- }}}

sense :: (Game m) => m [String]
sense = do
-- {{{
  let tunnels = asks maze <!> gets gCave

  nearTrevor <- elem <$> gets trevor <*> tunnels
  nearBats   <- liftA2 any (flip elem <$> asks bats) tunnels
  nearPits   <- liftA2 any (flip elem <$> asks pits) tunnels

  return $ filterByList [nearTrevor, nearBats, nearPits] [Msg.senseWumpus, Msg.senseBats, Msg.sensePits]
-- }}}

execute :: (Game m) => Action -> m [String]
execute a@(Move c) = do
  eCave <- emptyCave

  updateHistory a
  let logs = ["[DEBUG] Updated cave to Cave " ++ show c]
  modify (\s -> s { gCave = c })
  let logs' =  ("[DEBUG] Updated history with action " ++ show a) : logs
  getMoveEvent a >>= \case
      Just Wumpus -> (modify $ \s -> s { gameOver = Just Lose }) >> (return $ Msg.encounterWumpus : logs')
      Just Pit    -> (modify $ \s -> s { gameOver = Just Lose }) >> (return $ Msg.losePits : logs')
      Just Bat    -> (modify $ \s -> s { gCave = eCave }) >> (return $ Msg.encounterBats : ("[DEBUG] Updated history with action " ++ show eCave) : logs')
      Nothing     -> return logs'

execute a@(Shoot c) = do
-- {{{
  remainingArrows <- gets (pred . crookedArrows)
  updateHistory a
  let logs = ["[DEBUG] Updated history with action " ++ show a]
  modify (\s -> s { crookedArrows = remainingArrows })

  getShootEvent a >>= \case
    Just Kill      -> (modify $ \s -> s {gameOver = Just Win})  >> (return $ Msg.winWumpus : logs)
    Just OutOfAmmo -> (modify $ \s -> s {gameOver = Just Lose}) >> (return $ [Msg.missed, Msg.loseArrows] ++ logs)
    Nothing -> do
      trevor' <- anotherCave
      if trevor' == c
      then (modify $ \s -> s { gameOver = Just Lose }) >> (return $ Msg.loseWumpus : logs)
      else return $ Msg.missed : logs
-- }}}

updateHistory a = do
-- {{{
  history <- gets ((a :) . gHistory)
  modify (\s -> s { gHistory = history })
-- }}}

emptyCave :: (Game m) => m Cave
emptyCave = do
-- {{{
  hazards <- gets trevor <:> asks (bats <++> pits)
  cs <- filter (not . flip elem hazards) . vertices <$> asks maze

  rand <- gets gen
  let (c, gen')  = randomR (1, length cs) rand
  modify (\s -> s {gen = gen'})

  return $ cs !! (c-1)
-- }}}

anotherCave :: (Game m) => m Cave
anotherCave = do
-- {{{
  trev <- gets trevor
  cs <- filter (not . (trev ==)) . vertices <$> asks maze

  rand <- gets gen
  let (c, gen')  = randomR (1, length cs) rand
  modify (\s -> s {gen = gen'})

  return $ cs !! (c-1)
-- }}}

getMoveEvent :: Game m => Action -> m (Maybe MoveEvent)
getMoveEvent (Move c) = do
-- {{{
  pitFalls <- asks pits
  batCaves <- asks bats
  trev <- gets trevor
  return $
    toMaybe (trev == c) Wumpus
    <|> toMaybe (c `elem` pitFalls) Pit
    <|> toMaybe (c `elem` batCaves) Bat
-- }}}

getShootEvent :: Game m => Action -> m (Maybe ShootEvent)
getShootEvent (Shoot c) = do
-- {{{
  gs <- get
  return $
    toMaybe ((trevor gs) == c) Kill
    <|> toMaybe (0 == (crookedArrows gs)) OutOfAmmo
-- }}}
