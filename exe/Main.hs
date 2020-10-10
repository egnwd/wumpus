module Main where

import Wumpus

main :: IO ()
main = do
  putStrLn "Welcome to \"Hunt the Wumpus\"!!"
  gs <- runWumpus initialState initialWorld
  print gs
