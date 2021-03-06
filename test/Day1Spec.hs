module Day1Spec (specIO) where

import Test.Hspec
import qualified Day1 as Day1

spec :: String -> Spec
spec input =
  describe "Day1" $ do
    it "Santa reached floor 280 of the building" $ do
      Day1.advanceToFloor input `shouldBe` 280
    it "Requires 1797 steps to get to the basement" $ do
      Day1.stepsToBasement input `shouldBe` 1797

specIO :: IO Spec
specIO = do
  input <- readFile "test/Day1.txt"
  return (spec input)