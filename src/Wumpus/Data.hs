{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Wumpus.Data
  ( Cave(..)
  , Maze(..)
  , Action(..)
  , Result(..)

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
  , gen
  , initialState
  ) where

import Control.Lens
import Data.Graph
import System.Random

import Wumpus.Utils

type Cave = Int
type Maze = Graph

data WorldConfig = W
  { _maze    :: Maze
  , _pits    :: [Cave]
  , _bats    :: [Cave]
  , _isDebug :: Bool
  } deriving (Show)

makeLenses ''WorldConfig

data Action = Shoot Cave | Move Cave deriving (Show)
data Result = Win | Lose deriving (Show)

data GameState = GameState
  { _crookedArrows :: Int
  , _gCave         :: Cave
  , _gHistory      :: [Action]
  , _trevor        :: Cave
  , _gameOver      :: Maybe Result
  , _gen           :: StdGen
  }

instance Show GameState where
  show gs@GameState{ _gameOver = Just _ } =
    "You finished in cave " ++ show (_gCave gs) ++ ".\n"
    ++ "You have " ++ show (_crookedArrows gs) ++ " arrows remaining.\n"
    ++ "You made " ++ show (length $ _gHistory gs) ++ " steps.\n"
    ++ "Trevor, the Wumpus was in cave " ++ show (_trevor gs) ++ "."

  show GameState{ _gameOver = Nothing } = "NO PEEKING!"

makeLenses ''GameState

initialWorld :: WorldConfig
initialWorld = W
  { _maze   = initialMaze
  , _pits   = intitalPits
  , _bats   = initialBats
  , _isDebug = False
  }

hazards :: Getter WorldConfig [Cave]
hazards = runGetter $ Getter bats <++> Getter pits

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
  { _crookedArrows = 5
  , _gCave         = 1
  , _gHistory      = []
  , _trevor        = intialTrevor
  , _gameOver      = Nothing
  , _gen           = mkStdGen seed
  }
