module Day11Spec where

import Test.Hspec
import qualified Day11 as Day11

spec :: Spec
spec =
  describe "Day11" $ do
    it "Next valid password after vzbxkghb is vzbxxyzz" $ do
      Day11.nextValidPassword "vzbxkghb" `shouldBe` "vzbxxyzz"
    it "Next valid password after vzbxxyzz is vzcaabcc" $ do
      Day11.nextValidPassword "vzbxxyzz" `shouldBe` "vzcaabcc"

specIO :: IO Spec
specIO = do
  return spec