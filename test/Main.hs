module Main (main) where

import Test.Hspec
import Day1Spec as Day1
import Day2Spec as Day2
import Day3Spec as Day3
import Day4Spec as Day4
import Day5Spec as Day5

main :: IO ()
main = 
  do
    day1 <- Day1.specIO
    day2 <- Day2.specIO
    day3 <- Day3.specIO
    day4 <- Day4.specIO
    day5 <- Day5.specIO
    hspec $ do
      day1
      day2
      day3
      day4
      day5