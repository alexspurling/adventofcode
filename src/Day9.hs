module Day9 where

distanceMatrix :: [[Int]]
distanceMatrix =
  [[0  ,129, 58, 13, 24, 60, 71, 67],
   [129,  0,142, 15,135, 75, 82, 54],
   [ 58,142,  0,118,122,103, 49, 97],
   [ 13, 15,118,  0,116, 12, 18, 91],
   [ 24,135,122,116,  0,129, 53, 40],
   [ 60, 75,103, 12,129,  0, 15, 99],
   [ 71, 82, 49, 18, 53, 15,  0, 70],
   [ 67, 54, 97, 91, 40, 99, 70,  0]]

-- I'm aware of the Data.List.permutations function. This was my attempt to implement
-- a permutations algorithm
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) =
  foldr (++) [] (map (intersperse [] x) (permutations xs))
    where
      intersperse :: [a] -> a -> [a] -> [[a]]
      intersperse xs x [] = [xs ++ [x]]
      intersperse xs x (y:ys) =
        (xs ++ (x:y:ys)) : 
          (intersperse (xs ++ [y]) x ys)

hops route = zip route (tail route)

distances cityHops =
  map (\(from, to) -> (distanceMatrix!!from)!!to) cityHops

totalDistances =
  let
    numCities = length distanceMatrix - 1
    allroutes = permutations [0..numCities]
    allhops = map hops allroutes
    cityDistances = map distances allhops
  in
    map sum cityDistances

shortestRoute :: String -> Int
shortestRoute input =
  minimum totalDistances

longestRoute :: String -> Int
longestRoute input =
  maximum totalDistances