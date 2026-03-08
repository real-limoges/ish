module Main (main) where

import Test.Hspec (describe, hspec, it, shouldBe)

import Ish.Types (mkScore, unScore)

main :: IO ()
main = hspec $ do
  describe "Score smart constructor" $ do
    it "accepts 0" $
      fmap unScore (mkScore 0) `shouldBe` Just 0

    it "accepts 1" $
      fmap unScore (mkScore 1) `shouldBe` Just 1

    it "accepts 0.5" $
      fmap unScore (mkScore 0.5) `shouldBe` Just 0.5

    it "rejects negative values" $
      mkScore (-0.1) `shouldBe` Nothing

    it "rejects values above 1" $
      mkScore 1.1 `shouldBe` Nothing
