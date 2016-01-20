module Main (main) where

import Test.Hspec
import Day1Spec as Day1
import Day2Spec as Day2
import Day3Spec as Day3
import Day4Spec as Day4
import Day5Spec as Day5
import Day6Spec as Day6
import Day7Spec as Day7
import Day8Spec as Day8
import Day9Spec as Day9
import Day10Spec as Day10
import Day11Spec as Day11
import Day12Spec as Day12
import Day13Spec as Day13

main :: IO ()
main = 
  do
    day1 <- Day1.specIO
    day2 <- Day2.specIO
    day3 <- Day3.specIO
    day4 <- Day4.specIO
    day5 <- Day5.specIO
    day6 <- Day6.specIO
    day7 <- Day7.specIO
    day8 <- Day8.specIO
    day9 <- Day9.specIO
    day10 <- Day10.specIO
    day11 <- Day11.specIO
    day12 <- Day12.specIO
    day13 <- Day13.specIO
    hspec $ do
      day1
      day2
      day3
      day4
      day5
      day6
      day7
      day8
      day9
      day10
      day11
      day12
      day13