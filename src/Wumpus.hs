-- vim: fdm=marker
module Wumpus
  ( runWumpus
  , initialState
  , initialWorld
  ) where

--{{{
import Control.Applicative
import Data.Array
import Data.Graph
import Data.List
import System.Random
import Data.Maybe.HT


import Wumpus.Data
import Wumpus.Movement
import qualified Wumpus.Messages as Msg
-- }}}

runWumpus = loop

data MoveEvent  = Wumpus | Bat | Pit
data ShootEvent = Kill | OutOfAmmo

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
execute wc gs a@(Move c) =
  let event = getMoveEvent wc gs a
      (eCave, gs') = emptyCave wc gs
      gs'' = gs' {gCave = c, gHistory = a : (gHistory gs)}
   in case event of
        Just Wumpus -> ([Msg.encounterWumpus], gs'' {gameOver = Just Lose})
        Just Pit    -> ([Msg.losePits], gs'' {gameOver = Just Lose})
        Just Bat    -> ([Msg.encounterBats], gs'' {gCave = eCave})
        Nothing     -> ([], gs'')

execute wc gs a@(Shoot c) =
--{{{
  let remainingArrows = (crookedArrows gs) - 1
      gs' = gs { crookedArrows = remainingArrows }
      event = getShootEvent gs' a
      gs'' = gs' { gHistory = a : (gHistory gs) }
   in case event of
        Just Kill      -> ([Msg.winWumpus], gs'' {gameOver = Just Win})
        Just OutOfAmmo -> ([Msg.missed, Msg.loseArrows], gs'' {gameOver = Just Lose})
        Nothing -> let (newTrevor, gs''') = anotherCave wc gs''
                    in if newTrevor == c
                       then ([Msg.loseWumpus], gs''' {trevor = newTrevor, gameOver = Just Lose})
                       else ([Msg.missed], gs''' {trevor = newTrevor})
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

getMoveEvent :: WorldConfig -> GameState -> Action -> Maybe MoveEvent
getMoveEvent wc gs (Move c) = do
  toMaybe ((trevor gs) == c) Wumpus
  <|> toMaybe (c `elem` (pits wc)) Pit
  <|> toMaybe (c `elem` (bats wc)) Bat

getShootEvent :: GameState -> Action -> Maybe ShootEvent
getShootEvent gs (Shoot c) = do
  toMaybe ((trevor gs) == c) Kill
  <|> toMaybe (0 == (crookedArrows gs)) OutOfAmmo
