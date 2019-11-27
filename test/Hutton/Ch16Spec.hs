{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch16Spec where

-- import           Data.Char
import qualified Hutton.Ch13                   as P
import           Hutton.Ch16
import           Test.Hspec
-- import           Test.QuickCheck


spec :: Spec
spec = describe "Ch16" $ do
  describe "Misc" $ do
    it "epxr" $ P.parse expr "1 ab" `shouldBe` [(Val 1, "ab")]
    it "epxr" $ P.parse expr "1 + 3 ab" `shouldBe` [(Add (Val 1) (Val 3), "ab")]
    it "eval1" $ eval1 "1 + 3" `shouldBe` 4
    it "eval1" $ eval1 "1" `shouldBe` 1
    it "eval1" $ eval1 "1 + 3 + 2" `shouldBe` 6

    it "eval2" $ eval2 "1 + 3 + 2" `shouldBe` 6
    it "eval2" $ eval2 "1" `shouldBe` 1
    -- it "eval2" $ eval2 "1 + " `shouldBe` 1


