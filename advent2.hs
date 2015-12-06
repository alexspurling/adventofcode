import qualified Text.Regex as R
import qualified Data.List as L

main = do
  input <- readFile "advent2.txt"
  let totalArea = totalPaper input
  putStrLn ("Requires " ++ (show totalArea) ++ " sq ft of wrapping paper")

totalPaper :: String -> Int
totalPaper input =
  sum (map paperRequired (lines input))

paperRequired :: String -> Int
paperRequired present =
  let
    dimensions = R.splitRegex (R.mkRegex "x") present
    sortedDimensions = L.sort dimensions
  in
    case dimensions of
      (w:l:h:_) ->
        --Convert from List String to List Int
        calculateDimensions [read w, read l, read h]
      _ ->
        0

calculateDimensions :: [Int] -> Int
calculateDimensions [w, l, h] =
  let 
    presentArea = 2 * w * l + 2 * l * h + 2 * w * h
    [d1, d2] = smallestDimensions [w, l, h]
    extraArea = d1 * d2
  in
    presentArea + extraArea

smallestDimensions :: [Int] -> [Int]
smallestDimensions dimensions =
  L.take 2 (L.sort dimensions)