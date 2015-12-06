import qualified Text.Regex as R
import qualified Data.List as L

main = do
  input <- readFile "advent2.txt"
  let presents = getPresents input
  let totalArea = totalPaper presents
  let totalLength = totalRibbon presents
  putStrLn ("Requires " ++ (show totalArea) ++ " sq ft of wrapping paper")
  putStrLn ("Requires " ++ (show totalLength) ++ " ft of ribbon")

getPresents :: String -> [[Int]]
getPresents input =
  map getDimensions (lines input)

getDimensions :: String -> [Int]
getDimensions present =
  map read (R.splitRegex (R.mkRegex "x") present)

totalPaper :: [[Int]] -> Int
totalPaper presents =
  sum (map paperRequired presents) 

paperRequired :: [Int] -> Int
paperRequired [w, l, h] =
  let 
    presentArea = 2 * w * l + 2 * l * h + 2 * w * h
    [d1, d2] = smallestDimensions [w, l, h]
    extraArea = d1 * d2
  in
    presentArea + extraArea

totalRibbon :: [[Int]] -> Int
totalRibbon presents =
  sum (map ribbonRequired presents)

ribbonRequired :: [Int] -> Int
ribbonRequired [w, h, l] =
  let
    [d1, d2] = smallestDimensions [w, h, l]
    mainLength = 2 * d1 + 2 * d2
    bowLength = w * h * l
  in
    mainLength + bowLength

smallestDimensions :: [Int] -> [Int]
smallestDimensions dimensions =
  L.take 2 (L.sort dimensions)