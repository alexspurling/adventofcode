module Day2Spec (specIO) where

import Test.Hspec
import qualified Day2 as Day2

spec :: String -> Spec
spec input =
  describe "Day2" $ do
    it "Requires 1606483 sq ft of wrapping paper" $ do
      Day2.totalArea input `shouldBe` 1606483
    it "Requires 3842356 ft of ribbon" $ do
      Day2.totalLength input `shouldBe` 3842356

specIO :: IO Spec
specIO = do
  input <- readFile "test/Day2.txt"
  return (spec input)