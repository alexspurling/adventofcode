module Main (main) where

import Data.Foldable
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
import Day14Spec as Day14

main :: IO ()
main = do
  let days = [ Day1.specIO, Day2.specIO, Day3.specIO, Day4.specIO, Day5.specIO
             , Day6.specIO, Day7.specIO, Day8.specIO, Day9.specIO, Day10.specIO
             , Day11.specIO, Day12.specIO, Day13.specIO, Day14.specIO]
  daysIO <- sequenceA days
  hspec $ sequenceA_ daysIO