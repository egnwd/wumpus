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
import Control.Bool
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Array
import Data.Foldable
import Data.Graph
import Data.List
import Data.Maybe.HT
import Prelude hiding (putStrLn)
import System.Random

import Wumpus.Data
import Wumpus.Movement
import Wumpus.Utils
import qualified Wumpus.Messages as Msg
-- }}}

runWumpus :: MonadIO m => GameState -> WorldConfig -> m GameState
runWumpus = runReaderT . execStateT loop

data MoveEvent  = Wumpus | Bat | Pit
data ShootEvent = Kill | OutOfAmmo

type Game m = (MonadState GameState m, MonadReader WorldConfig m)
type Logging = WriterT [String]

loop :: (Game m, MonadIO m) => m ()
loop = do
-- {{{
  m <- asks maze
  cave <- gets gCave
  let tunnels = sort (m!cave)

  -- Cave info
  putStrLn $ Msg.youAreInCave cave
  execWriterT sense >>= traverse_ putStrLn
  putStrLn (Msg.tunnelsLeadTo tunnels)
  action <- getAction tunnels

  -- Player action
  d <- asks isDebug
  let shouldDebug l = d || (not $ isPrefixOf "[DEBUG]" l)
  logs <- execWriterT $ censor (filter shouldDebug) (execute action)
  traverse_ putStrLn logs

  gets gameOver >>= \case
      Nothing -> loop
      Just Win -> putStrLn Msg.win
      Just Lose -> putStrLn Msg.lose
-- }}}

sense :: (Game m) => Logging m ()
sense = do
-- {{{
  let tunnels = asks maze <!> gets gCave
  whenM (elem <$> gets trevor <*> tunnels)       $ logMsg Msg.senseWumpus
  whenM (anyM (flip elem <$> asks bats) tunnels) $ logMsg Msg.senseBats
  whenM (anyM (flip elem <$> asks pits) tunnels) $ logMsg Msg.sensePits
-- }}}

execute :: (Game m) => Action -> Logging m ()

execute a@(Move c) = do
  eCave <- emptyCave

  updateHistory a >> (logDebug $ "Updated cave to Cave " ++ show c)
  modify (\s -> s { gCave = c }) >> (logDebug $ "Updated history with action " ++ show a)

  getMoveEvent a >>= \case
      Just Wumpus -> (modify $ \s -> s { gameOver = Just Lose }) >> logMsg Msg.encounterWumpus
      Just Pit    -> (modify $ \s -> s { gameOver = Just Lose }) >> logMsg Msg.losePits
      Just Bat    -> do
        modify $ \s -> s { gCave = eCave }
        logDebug $ "Updated history with action " ++ show a
        logMsg Msg.encounterBats
      Nothing     -> return ()

execute a@(Shoot c) = do
-- {{{
  remainingArrows <- gets (pred . crookedArrows)
  updateHistory a >> (logDebug $ "Updated history with action " ++ show a)
  modify (\s -> s { crookedArrows = remainingArrows })

  getShootEvent a >>= \case
    Just Kill      -> (modify $ \s -> s {gameOver = Just Win})  >> logMsg Msg.winWumpus
    Just OutOfAmmo -> (modify $ \s -> s {gameOver = Just Lose}) >> logMsgs [Msg.missed, Msg.loseArrows]
    Nothing -> do
      trevor' <- anotherCave
      if trevor' == c
      then (modify $ \s -> s { gameOver = Just Lose }) >> logMsg Msg.loseWumpus
      else logMsg Msg.missed
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
