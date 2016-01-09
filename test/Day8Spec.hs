module Day8Spec (specIO) where

import Test.Hspec
import Day8

spec :: String -> Spec
spec input =
  describe "Day8" $ do
    it "Character size difference should be 123" $ do
      Day8.characterDifference input `shouldBe` 123

specIO :: IO Spec
specIO = do
  input <- readFile "test/Day8.txt"
  return (spec input)