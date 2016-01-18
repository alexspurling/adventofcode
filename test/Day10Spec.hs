module Day10Spec where

import Test.Hspec
import qualified Day10 as Day10

spec :: Spec
spec =
  describe "Day10" $ do
    it "Look and say length with 40 iterations is 252594" $ do
      Day10.lookAndSayLength "1113222113" 40 `shouldBe` 252594
    it "Look and say length with 50 iterations is 3579328" $ do
      Day10.lookAndSayLength "1113222113" 50 `shouldBe` 3579328

specIO :: IO Spec
specIO = return spec