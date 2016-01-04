module Day3 where

import qualified Data.Set as Set

getNumberOfHouses :: String -> Int
getNumberOfHouses input =
  let
    visits = visitHouses (0, 0) Set.empty input
  in
    countVisits visits

getRoboNumberOfHouses :: String -> Int
getRoboNumberOfHouses input =
  let
    --Santa follows every odd direction
    santaDirections = each 2 input
    --Robo santa follows every even direction
    roboDirections = each 2 $ tail input
    santaVisits = visitHouses (0, 0) Set.empty santaDirections
    roboSantaVisits = visitHouses (0, 0) santaVisits roboDirections
  in
    countVisits roboSantaVisits

type VisitSet = Set.Set (Int,Int)

visitHouses :: (Int, Int) -> VisitSet -> String -> VisitSet
visitHouses position visits "" =
  visits

visitHouses (x, y) visits (nextDirection:remainingDirections) =
  let
    --Mark this position as visited
    newVisits = Set.insert (x, y) visits
    nextPosition =
      case nextDirection of
        '<' ->
          (x-1, y)
        '>' ->
          (x+1, y)
        '^' ->
          (x, y-1)
        'v' ->
          (x, y+1)
  in
    visitHouses nextPosition newVisits remainingDirections

countVisits :: VisitSet -> Int
countVisits visits =
  Set.size visits

each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)