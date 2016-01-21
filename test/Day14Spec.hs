module Day14Spec where

import Test.Hspec
import Day14

spec :: Spec
spec =
  describe "Day14" $ do
    it "Maximum reindeer distance is 2655" $ do
      Day14.maximumDistance `shouldBe` 2655
    it "Maximum reindeer points is 1059" $ do
      Day14.maximumPoints `shouldBe` 1059

specIO :: IO Spec
specIO = return spec