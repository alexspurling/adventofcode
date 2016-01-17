module Day10 where

import Data.List

lookAndSay :: String -> String
lookAndSay string =
  --Group chars in string
  --Then count the number of elements in each group
  --Add the count followed by the element to new list
  concat $ map (\g -> (head g) : show (length g)) (group string)

nthLookAndSay :: String -> Int -> String
nthLookAndSay input n = (iterate lookAndSay input) !! n

lookAndSayLength :: String -> Int -> Int
lookAndSayLength input n = length $ nthLookAndSay (reverse input) n