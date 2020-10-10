-- vim: fdm=marker
module Wumpus
  ( runWumpus
  , initialState
  , initialWorld
  ) where

--{{{
import Data.Array
import Data.Graph
import Data.List
import System.Random

import Wumpus.Data
import Wumpus.Movement
import qualified Wumpus.Messages as Msg
-- }}}

runWumpus = loop

loop :: WorldConfig -> GameState -> IO GameState
loop wc gs = do
--{{{
  let m = maze wc
  let cave = gCave gs
  let tunnels = sort (m ! cave)

  -- Cave info
  putStrLn $ Msg.youAreInCave cave
  mapM_ putStrLn $ sense wc gs
  putStrLn $ Msg.tunnelsLeadTo tunnels

  -- Player action
  action <- getAction tunnels
  let (logs, gs') = execute wc gs action
  mapM_ putStrLn logs

  case gameOver gs' of
    Nothing -> loop wc gs'
    Just Win -> putStrLn Msg.win >> return gs'
    Just Lose -> putStrLn Msg.lose >> return gs'
-- }}}

sense :: WorldConfig -> GameState -> [String]
sense wc gs = trevorE $ batsE $ pitsE []
  where
--{{{
    tunnels = (maze wc ! gCave gs)
    trevorE = if (trevor gs) `elem` tunnels then ([Msg.senseWumpus] ++) else ([] ++)
    batsE = if any (flip elem (bats wc)) tunnels then ([Msg.senseBats] ++) else ([] ++)
    pitsE = if any (flip elem (pits wc)) tunnels then ([Msg.sensePits] ++) else ([] ++)
-- }}}

-- | Execute the Player's Action
execute :: WorldConfig -> GameState -> Action -> ([String], GameState)
execute wc gs a@(Move c)
  | (trevor gs) == c = ([Msg.encounterWumpus], gs'' {gameOver = Just Lose})
  | c `elem` (pits wc) = ([Msg.losePits], gs'' {gameOver = Just Lose})
  | c `elem` (bats wc) = ([Msg.encounterBats], gs'' {gCave = eCave})
  | otherwise = ([], gs'')
  where
    (eCave, gs') = emptyCave wc gs
    gs'' = gs' {gCave = c, gHistory = a : (gHistory gs)}

execute wc gs a@(Shoot c)
--{{{
  | (trevor gs) == c = ([Msg.winWumpus], gs'' {gameOver = Just Win})
  | remainingArrows == 0 = ([Msg.missed, Msg.loseArrows], gs'' {gameOver = Just Lose})
  | newTrevor == c = ([Msg.loseWumpus], gs'' {trevor = newTrevor, gameOver = Just Lose})
  | otherwise = ([Msg.missed], gs'' {trevor = newTrevor})
  where
    remainingArrows = (crookedArrows gs) - 1
    (newTrevor, gs') = anotherCave wc gs
    gs'' = gs' {crookedArrows = remainingArrows, gHistory = a : (gHistory gs)}
-- }}}

-- | Helper Functions
emptyCave :: WorldConfig -> GameState -> (Cave, GameState)
emptyCave wc gs =
--{{{
  let cs = vertices $ maze wc
      hazards = (trevor gs) : (bats wc) ++ (pits wc)
      validCaves = filter (not . flip elem hazards) cs
      (c, gen') = randomR (1, length validCaves) (gen gs)
   in (validCaves !! (c -1), gs {gen = gen'})
-- }}}

anotherCave :: WorldConfig -> GameState -> (Cave, GameState)
anotherCave wc gs =
--{{{
  let cs = vertices $ maze wc
      validCaves = filter (not . (trevor gs ==)) cs
      (c, gen') = randomR (1, length validCaves) (gen gs)
   in (validCaves !! (c -1), gs {gen = gen'})
-- }}}

