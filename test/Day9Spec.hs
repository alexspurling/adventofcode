module Day9Spec where

import Test.Hspec
import Day9

spec :: String -> Spec
spec input =
  describe "Day9" $ do
    it "Distance of shortest route should be 207" $ do
      Day9.shortestRoute input `shouldBe` 207
    it "Distance of longest route should be 804" $ do
      Day9.longestRoute input `shouldBe` 804

specIO :: IO Spec
specIO = do
  input <- readFile "test/Day9.txt"
  return (spec input)