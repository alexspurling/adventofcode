module Day6Spec (specIO) where

import Test.Hspec
import qualified Day6 as Day6

spec :: String -> Spec
spec input =
  describe "Day6" $ do
    it "There are 569999 lights lit." $ do
      Day6.lightsLit input `shouldBe` 569999
    it "Total light grid brightness is 123." $ do
      Day6.totalBrightness input `shouldBe` 17836115

specIO :: IO Spec
specIO = do
  input <- readFile "test/Day6.txt"
  return (spec input)