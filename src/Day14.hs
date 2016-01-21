module Day14 where

import Data.Bool
import Data.List

data Reindeer = Reindeer { name :: String, speed :: Int, stamina :: Int, rest :: Int }

reindeer = [
      Reindeer "Vixen" 8 8 53
    , Reindeer "Blitzen" 13 4 49
    , Reindeer "Rudolph" 20 7 132
    , Reindeer "Cupid" 12 4 43
    , Reindeer "Donner" 9 5 38
    , Reindeer "Dasher" 10 4 37
    , Reindeer "Comet" 3 37 76
    , Reindeer "Prancer" 9 12 97
    , Reindeer "Dancer" 37 1 36
  ]

{-- what distance has this reindeer reached after x seconds? --}
distance :: Int -> Reindeer -> Int
distance seconds reindeer =
  let
    cycleLength = rest reindeer + stamina reindeer
    numCycles = seconds `div` cycleLength
    remainingSeconds = seconds `mod` cycleLength
    wholeCycleDistance = numCycles * speed reindeer * stamina reindeer
    remainingDistance = speed reindeer * (min (stamina reindeer) remainingSeconds)
  in
    wholeCycleDistance + remainingDistance

maximumDistance :: Int
maximumDistance =
  maximum $ map (distance 2503) reindeer

points :: Int -> [Int]
points seconds =
  let
    distances = map (distance seconds) reindeer
    maxDistance = maximum distances
    wonPoints = map ((==) maxDistance) distances
    points = map (bool 0 1) wonPoints
  in
    points

initialPoints :: [Int]
initialPoints =
  replicate (length reindeer) 0

totalPoints :: Int -> [Int]
totalPoints seconds =
  foldr (zipWith (+)) initialPoints (map points [1..seconds])

maximumPoints :: Int
maximumPoints =
  maximum $ totalPoints 2503