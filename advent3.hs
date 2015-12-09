import Data.Map as M

main = do
  input <- readFile "advent3.txt"
  let numberOfHouses = getNumberOfHouses input
  putStrLn ("Santa visited " ++ (show numberOfHouses) ++ " lucky people this year")

getNumberOfHouses :: String -> Int
getNumberOfHouses input =
  let
    visitMap = visitHouses (0, 0) M.empty input
  in
    countVisits visitMap

type VisitMatrix = M.Map Int (M.Map Int Int)

visitHouses :: (Int, Int) -> VisitMatrix -> String -> VisitMatrix
visitHouses position visitMap "" =
  visitMap

visitHouses (x, y) visitMap (nextDirection:remainingDirections) =
  let
    --Increment the visit count in the current position
    yMap = M.findWithDefault M.empty x visitMap :: M.Map Int Int
    newYMap = M.insertWith (+) y 1 yMap
    newVisitMap = M.insert x newYMap visitMap
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
    visitHouses nextPosition newVisitMap remainingDirections

countVisits :: VisitMatrix -> Int
countVisits visitMap =
  M.foldr (\m visitCount -> (M.size m + visitCount)) 0 visitMap
