module Day5 where

import Data.List
import Data.Maybe
import Text.Regex

niceStrings input =
  countNice input isNice

niceStrings2 input =
  countNice input isNice2

countNice :: String -> (String -> Bool) -> Int
countNice input niceFunction =
  length (filter niceFunction (lines input))

isNice :: String -> Bool
isNice string =
  (hasThreeVowels string) && (doubleLetters string) && (not (hasBadSubstring string))

hasThreeVowels :: String -> Bool
hasThreeVowels string =
  length (filter isVowel string) > 2

isVowel :: Char -> Bool
isVowel char =
  elem char "aeiou"

doubleLetters :: String -> Bool
doubleLetters string =
  length (group string) < (length string)

badSubstrings :: [String]
badSubstrings = ["ab", "cd", "pq", "xy"]

hasBadSubstring :: String -> Bool
hasBadSubstring string =
  any (\x -> isInfixOf x string) badSubstrings

isNice2 :: String -> Bool
isNice2 string =
  let
    hasDoublePair = isJust (matchRegex (mkRegex "(..).*\\1") string)
    hasSurroundChar = isJust (matchRegex (mkRegex "(.).\\1") string)
  in
    hasDoublePair && hasSurroundChar