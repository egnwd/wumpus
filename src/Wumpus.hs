-- vim: fdm=marker
{-# LANGUAGE FlexibleContexts, MultiWayIf, ConstraintKinds #-}
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
import Data.Array
import Data.Graph
import Data.List
import Prelude hiding (putStrLn)
import System.Random
import Data.Maybe.HT
import Data.Foldable

import Wumpus.Data
import Wumpus.Movement
import qualified Wumpus.Messages as Msg
import Wumpus.Utils
-- }}}

runWumpus :: MonadIO m => GameState -> WorldConfig -> m GameState
runWumpus = runReaderT . loop

data MoveEvent  = Wumpus | Bat | Pit
data ShootEvent = Kill | OutOfAmmo

type World m = MonadReader WorldConfig m

loop :: (World m, MonadIO m) => GameState -> m GameState
loop gs = do
-- {{{
  m <- asks maze
  let cave = gCave gs
  let tunnels = sort (m ! cave)

  -- Cave info
  putStrLn $ Msg.youAreInCave cave
  sense gs >>= traverse_ putStrLn
  putStrLn (Msg.tunnelsLeadTo tunnels)

  -- Player action
  action <- getAction tunnels
  d <- asks isDebug
  let shouldDebug l = d || (not $ isPrefixOf "[DEBUG]" l)
  (gs', logs) <- execute action gs
  traverse_ putStrLn $ filter shouldDebug logs

  case gameOver gs' of
    Nothing -> loop gs'
    Just Win -> putStrLn Msg.win >> return gs'
    Just Lose -> putStrLn Msg.lose >> return gs'
-- }}}

sense :: World m => GameState -> m [String]
sense gs = do
-- {{{
  tunnels <- (\m -> m ! gCave gs) <$> asks maze
  batCaves <- asks bats
  pitFalls <- asks pits

  let trevorE = if (trevor gs) `elem` tunnels then ([Msg.senseWumpus] ++) else ([] ++)
  let batsE   = if any (flip elem batCaves) tunnels then ([Msg.senseBats] ++) else ([] ++)
  let pitsE   = if any (flip elem pitFalls) tunnels then ([Msg.sensePits] ++) else ([] ++)

  return $ trevorE $ batsE $ pitsE []
-- }}}

execute :: World m => Action -> GameState -> m (GameState, [String])
execute a@(Move c) gs = do
  (gs', eCave) <- emptyCave gs
  let gs'' = gs' { gCave = c, gHistory = a : (gHistory gs) }
      logs = ["[DEBUG] Updated cave to Cave " ++ show c, "[DEBUG] Updated history with action " ++ show a]

  getMoveEvent a gs >>= \event -> return $
    case event of
      Just Wumpus -> (gs'' { gameOver = Just Lose }, Msg.encounterWumpus : logs)
      Just Pit    -> (gs'' { gameOver = Just Lose }, Msg.losePits : logs)
      Just Bat    -> (gs'' { gCave = eCave },        Msg.encounterBats : ("[DEBUG] Updated cave to Cave " ++ show eCave) : logs)
      Nothing     -> (gs'', logs)

execute a@(Shoot c) gs = do
-- {{{
  let remainingArrows = (crookedArrows gs) - 1
      gs' = gs { crookedArrows = remainingArrows, gHistory = a : (gHistory gs)}
      logs = ["[DEBUG] Updated history with action " ++ show a]

  let event = getShootEvent a gs'
  case event of
    Just Kill      -> return (gs' {gameOver = Just Win},  Msg.winWumpus : logs)
    Just OutOfAmmo -> return (gs' {gameOver = Just Lose}, [Msg.missed, Msg.loseArrows] ++ logs)
    Nothing -> do
      (gs'', trevor') <- anotherCave gs'
      let gs''' = gs'' { trevor = trevor' }
      if trevor' == c
      then return (gs''' {gameOver = Just Lose}, Msg.loseWumpus : logs)
      else return (gs''', Msg.missed : logs)
-- }}}

emptyCave :: World m => GameState -> m (GameState, Cave)
emptyCave gs = do
-- {{{
  hazards <- (trevor gs :) <$> asks (\r -> bats r ++ pits r)
  cs <- vertices <$> asks maze

  let validCaves = filter (not . flip elem hazards) cs
      (c, gen')  = randomR (1, length validCaves) (gen gs)

  return (gs { gen = gen' }, validCaves !! (c-1))
-- }}}

anotherCave :: World m => GameState -> m (GameState, Cave)
anotherCave gs = do
-- {{{
  cs <- vertices <$> asks maze
  let validCaves = filter (not . (trevor gs ==)) cs
      (c, gen')  = randomR (1, length validCaves) (gen gs)

  return (gs { gen = gen' }, validCaves !! (c-1))
-- }}}

getMoveEvent :: World m => Action -> GameState -> m (Maybe MoveEvent)
getMoveEvent (Move c) gs = do
-- {{{
  pitFalls <- asks pits
  batCaves <- asks bats
  return $
    toMaybe ((trevor gs) == c) Wumpus
    <|> toMaybe (c `elem` pitFalls) Pit
    <|> toMaybe (c `elem` batCaves) Bat
-- }}}

getShootEvent :: Action -> GameState -> Maybe ShootEvent
getShootEvent (Shoot c) gs = do
-- {{{
  toMaybe ((trevor gs) == c) Kill
  <|> toMaybe (0 == (crookedArrows gs)) OutOfAmmo
-- }}}
