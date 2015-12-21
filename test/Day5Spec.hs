module Day5Spec (specIO) where

import Test.Hspec
import qualified Day5 as Day5

spec :: String -> Spec
spec input =
  describe "Day5" $ do
    it "There are 255 nice strings." $ do
      Day5.niceStrings input `shouldBe` 255
    it "There are 55 nice strings with the new rules." $ do
      Day5.niceStrings2 input `shouldBe` 55

specIO :: IO Spec
specIO = do
  input <- readFile "test/Day5.txt"
  return (spec input)