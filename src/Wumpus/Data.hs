module Wumpus.Data
  ( Cave(..)
  , Maze(..)
  , WorldConfig(..)
  , Action(..)
  , Result(..)
  , GameState(..)
  , initialWorld
  , initialState
  ) where

import Data.Graph
import System.Random

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
  , gen           :: StdGen
  }

instance Show GameState where
  show gs@GameState{ gameOver = Just x } =
    "You finished in cave " ++ show (gCave gs) ++ ".\n"
    ++ "You have " ++ show (crookedArrows gs) ++ " arrows remaining.\n"
    ++ "You made " ++ show (length $ gHistory gs) ++ " steps.\n"
    ++ "Trevor, the Wumpus was in cave " ++ show (trevor gs) ++ "."

  show gs@GameState{ gameOver = Nothing } = "NO PEEKING!"

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

initialState :: Int -> GameState
initialState seed = GameState
  { crookedArrows = 5
  , gCave         = 1
  , gHistory      = []
  , trevor        = intialTrevor
  , gameOver      = Nothing
  , gen           = mkStdGen seed
  }
