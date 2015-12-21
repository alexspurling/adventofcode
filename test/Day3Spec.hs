module Day3Spec (specIO) where

import Test.Hspec
import qualified Day3 as Day3

spec :: String -> Spec
spec input =
  describe "Day3" $ do
    it "Santa visited 2081 lucky people this year" $ do
      Day3.getNumberOfHouses input `shouldBe` 2081
    it "Santa and robo santa visited 2341 lucky people this year" $ do
      Day3.getRoboNumberOfHouses input `shouldBe` 2341

specIO :: IO Spec
specIO = do
  input <- readFile "test/Day3.txt"
  return (spec input)