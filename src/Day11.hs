module Day11 where

import Data.Char
import Data.List

nextString :: String -> String
nextString [] = "a"
nextString str =
  let
    lastChar = last str
    lastCharPlus1 = chr (ord lastChar + 1) 
  in
    case lastChar of
      'z' -> nextString (init str) ++ "a"
      _ -> init str ++ [lastCharPlus1]

allPasswords :: String -> [String]
--Use tail so that we don't get the input as the first item
allPasswords startingPassword = tail $ iterate nextString startingPassword

hasIOL :: String -> Bool
hasIOL str =
  (elem 'i' str) || 
  (elem 'o' str) || 
  (elem 'l' str)

windows :: Int -> [a] -> [[a]]
windows size [] = []
windows size ls@(x:xs) = 
  if length ls >= size then 
    (take size ls) : windows size xs 
  else windows size xs

isStrictlyIncreasing :: (Eq a, Num a) => [a] -> Bool
isStrictlyIncreasing [] = True
isStrictlyIncreasing [x] = True
isStrictlyIncreasing (x:y:xs) = (y-x) == 1 && isStrictlyIncreasing (y:xs)

has3IncreasingLetters :: String -> Bool
has3IncreasingLetters str =
  any isStrictlyIncreasing (windows 3 (map ord str))

has2LetterGroups :: String -> Bool
has2LetterGroups str =
  length (filter (\x -> (length x >= 2)) (group str)) >= 2

validPassword :: String -> Bool
validPassword str =
  (not (hasIOL str)) &&
  has3IncreasingLetters str &&
  has2LetterGroups str

validPasswords :: String -> [String]
validPasswords startingPassword =
  filter validPassword (allPasswords startingPassword)

nextValidPassword :: String -> String
nextValidPassword startingPassword =
  head $ validPasswords startingPassword