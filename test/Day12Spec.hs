module Day12Spec where

import Test.Hspec
import qualified Day12 as Day12

spec :: String -> Spec
spec input =
  describe "Day12" $ do
    it "Sum of all numbers is 191164" $ do
      Day12.sumOfAllNumbers input `shouldBe` 191164
    it "Sum of all numbers except red ones is 87842" $ do
      Day12.sumOfNonRedNumbers input `shouldBe` 87842

specIO :: IO Spec
specIO = do
  input <- readFile "test/Day12.json"
  return (spec input)