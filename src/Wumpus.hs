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

loop :: GameState -> WorldConfig -> IO GameState
loop gs wc = do
--{{{
  let m = maze wc
  let cave = gCave gs
  let tunnels = sort (m ! cave)

  -- Cave info
  putStrLn $ Msg.youAreInCave cave
  mapM_ putStrLn $ sense gs wc
  putStrLn $ Msg.tunnelsLeadTo tunnels

  -- Player action
  action <- getAction tunnels
  let (gs', logs) = execute action gs wc
  mapM_ putStrLn logs

  case gameOver gs' of
    Nothing -> loop gs' wc
    Just Win -> putStrLn Msg.win >> return gs'
    Just Lose -> putStrLn Msg.lose >> return gs'
-- }}}

sense :: GameState -> WorldConfig -> [String]
sense gs wc = trevorE $ batsE $ pitsE []
  where
--{{{
    tunnels = (maze wc ! gCave gs)
    trevorE = if (trevor gs) `elem` tunnels then ([Msg.senseWumpus] ++) else ([] ++)
    batsE = if any (flip elem (bats wc)) tunnels then ([Msg.senseBats] ++) else ([] ++)
    pitsE = if any (flip elem (pits wc)) tunnels then ([Msg.sensePits] ++) else ([] ++)
-- }}}

-- | Execute the Player's Action
execute :: Action -> GameState -> WorldConfig -> (GameState, [String])
execute a@(Move c) gs wc =
  let event = getMoveEvent a gs wc
      (gs', eCave) = emptyCave gs wc
      gs'' = gs' {gCave = c, gHistory = a : (gHistory gs)}
   in case event of
        Just Wumpus -> (gs'' {gameOver = Just Lose}, [Msg.encounterWumpus])
        Just Pit    -> (gs'' {gameOver = Just Lose}, [Msg.losePits])
        Just Bat    -> (gs'' {gCave = eCave}, [Msg.encounterBats])
        Nothing     -> (gs'', [])

execute a@(Shoot c) gs wc =
--{{{
  let remainingArrows = (crookedArrows gs) - 1
      gs' = gs { crookedArrows = remainingArrows }
      event = getShootEvent a gs'
      gs'' = gs' { gHistory = a : (gHistory gs) }
   in case event of
        Just Kill      -> (gs'' {gameOver = Just Win}, [Msg.winWumpus])
        Just OutOfAmmo -> (gs'' {gameOver = Just Lose}, [Msg.missed, Msg.loseArrows])
        Nothing -> let (gs''', trevor') = anotherCave gs'' wc
                    in if trevor' == c
                       then (gs''' {trevor = trevor', gameOver = Just Lose}, [Msg.loseWumpus])
                       else (gs''' {trevor = trevor'}, [Msg.missed])
-- }}}

-- | Helper Functions
emptyCave :: GameState -> WorldConfig -> (GameState, Cave)
emptyCave gs wc =
--{{{
  let cs = vertices $ maze wc
      hazards = (trevor gs) : (bats wc) ++ (pits wc)
      validCaves = filter (not . flip elem hazards) cs
      (c, gen') = randomR (1, length validCaves) (gen gs)
   in (gs {gen = gen'}, validCaves !! (c -1))
-- }}}

anotherCave :: GameState -> WorldConfig -> (GameState, Cave)
anotherCave gs wc =
--{{{
  let cs = vertices $ maze wc
      validCaves = filter (not . (trevor gs ==)) cs
      (c, gen') = randomR (1, length validCaves) (gen gs)
   in ( gs {gen = gen'}, validCaves !! (c -1))
-- }}}

getMoveEvent :: Action -> GameState -> WorldConfig -> Maybe MoveEvent
getMoveEvent (Move c) gs wc = do
  toMaybe ((trevor gs) == c) Wumpus
  <|> toMaybe (c `elem` (pits wc)) Pit
  <|> toMaybe (c `elem` (bats wc)) Bat

getShootEvent :: Action -> GameState -> Maybe ShootEvent
getShootEvent (Shoot c) gs = do
  toMaybe ((trevor gs) == c) Kill
  <|> toMaybe (0 == (crookedArrows gs)) OutOfAmmo
