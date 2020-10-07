{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Reader
import Data.Array
import Data.Char
import Data.Functor.Syntax
import Data.Graph
import Data.List

import qualified Messages as Msg


main :: IO ()
main = do
  putStrLn "Welcome to \"Hunt the Wumpus\"!!"
  gs <- loop initialWorld initialState
  print gs

type Cave = Int
type Maze = Graph

data WorldConfig = W
  { maze   :: Maze
  , pits   :: [Cave]
  , bats   :: [Cave]
  } deriving (Show)

data Action = Shoot Cave | Move Cave deriving (Show)
data Result = Win | Lose deriving (Show)

data GameState = GameState
  { crookedArrows :: Int
  , gCave         :: Cave
  , gHistory      :: [Action]
  , trevor        :: Cave
  , gameOver      :: Maybe Result
  } deriving (Show)

initialWorld :: WorldConfig
initialWorld = W
  { maze   = initialMaze
  , pits   = intitalPits
  , bats   = initialBats
  }

initialMaze :: Maze
initialMaze = buildG (1,20)
  [ (1,2), (1,5), (1,8)
  , (2,1), (2,3), (2,10)
  , (3,2), (3,4), (3,12)
  , (4,3), (4,5), (4,14)
  , (5,1), (5,4), (5,6)
  , (6,5), (6,7), (6,15)
  , (7,6), (7,8), (7,17)
  , (8,7), (8,9), (8,1)
  , (9,8), (9,10), (9,18)
  , (10,9), (10,2), (10,11)
  , (11,10), (11,12), (11,19)
  , (12,11), (12,3), (12,13)
  , (13,12), (13,14), (13,20)
  , (14,13), (14,15), (14,4)
  , (15,14), (15,16), (15,6)
  , (16,15), (16,20), (16,17)
  , (17,16), (17,7), (17,18)
  , (18,17), (18,9), (18,19)
  , (19,18), (19,11), (19,20)
  , (20,19), (20,13), (20,16)
  ]

intialTrevor :: Cave
intitalPits, initialBats :: [Cave]
intialTrevor = 20
intitalPits  = [12, 8]
initialBats  = [2, 17]

initialState :: GameState
initialState = GameState
  { crookedArrows = 5
  , gCave         = 1
  , gHistory      = []
  , trevor        = intialTrevor
  , gameOver      = Nothing
  }

printM_ :: (MonadIO m, Foldable t) => t String -> m ()
printM_ = liftIO . mapM_ putStrLn

loop :: WorldConfig -> GameState -> IO GameState
loop wc gs = do
  let m = maze wc
  let cave = gCave gs
  let tunnels = sort (m!cave)

  -- Cave info
  putStrLn $ Msg.youAreInCave cave
  mapM_ putStrLn $ sense wc gs cave
  putStrLn $ Msg.tunnelsLeadTo tunnels

  -- Player action
  action <- getAction' tunnels
  let (logs, gs') = execute wc gs action
  mapM_ putStrLn logs

  case gameOver gs' of
    Nothing -> loop wc gs'
    Just Win -> putStrLn Msg.win >> return gs'
    Just Lose -> putStrLn Msg.lose >> return gs'

getAction' :: [Cave] -> IO Action
getAction' ts = getAction <*> getCave ts

getAction :: IO (Cave -> Action)
getAction = do
  putStrLn $ Msg.moveOrShoot
  action <- toLower <$$> getLine
  case action of
    "m" -> return Move
    "s" -> return Shoot
    _   -> putStrLn Msg.moveOrShootInstr >> getAction

getCave :: [Cave] -> IO Cave
getCave tunnels = do
  putStrLn $ Msg.whereTo
  cave <- readLn :: IO Cave
  if cave `elem` tunnels
     then return cave
     else do
       putStrLn $ Msg.whereToInstr tunnels
       getCave tunnels

sense :: WorldConfig -> GameState -> Cave -> [String]
sense wc gs c = trevorE $ batsE $ pitsE []
  where
    tunnels = (maze wc ! c)
    trevorE = if (trevor gs) `elem` tunnels then ([Msg.senseWumpus] ++) else ([] ++)
    batsE   = if any (flip elem (bats wc)) tunnels then ([Msg.senseBats] ++) else ([] ++)
    pitsE   = if any (flip elem (pits wc)) tunnels then ([Msg.sensePits] ++) else ([] ++)

execute :: WorldConfig -> GameState -> Action -> ([String], GameState)
execute wc gs a@(Move c)
  | (trevor gs) == c   = ([Msg.encounterWumpus], gs' { gameOver = Just Lose })
  | c `elem` (pits wc) = ([Msg.losePits], gs' { gameOver = Just Lose })
  | c `elem` (bats wc) = ([Msg.encounterBats], gs { gCave = emptyCave wc gs })
  | otherwise          = ([], gs')
  where
    gs' = gs { gCave = c, gHistory = a : (gHistory gs) }

execute wc gs a@(Shoot c)
  | (trevor gs) == c     = ([Msg.winWumpus], gs' { gameOver = Just Win })
  | remainingArrows == 0 = ([Msg.missed, Msg.loseArrows], gs' { gameOver = Just Lose })
  | newTrevor == c       = ([Msg.loseWumpus], gs' { trevor = newTrevor, gameOver = Just Lose })
  | otherwise            = ([Msg.missed], gs' { trevor = newTrevor })
  where
    remainingArrows = (crookedArrows gs) - 1
    newTrevor       = anotherCave wc gs
    gs' = gs { crookedArrows = remainingArrows, gHistory = a : (gHistory gs)}

emptyCave :: WorldConfig -> GameState -> Cave
emptyCave wc gs =
  let cs      = vertices $ maze wc
      hazards = (trevor gs) : (bats wc) ++ (pits wc)
   in head $ filter (not . flip elem hazards) cs

anotherCave :: WorldConfig -> GameState -> Cave
anotherCave wc gs =
  let cs      = vertices $ maze wc
   in head $ filter (not . (trevor gs ==)) cs
