{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Wumpus

main :: IO ()
main = do
  putStrLn "Welcome to \"Hunt the Wumpus\"!!"
  seed <- (round . (* 1000)) <$> getPOSIXTime
  gs <- runWumpus initialWorld (initialState seed)
  print gs

