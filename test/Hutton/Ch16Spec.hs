{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch16Spec where

-- import           Data.Char
import           Control.Exception
import qualified Hutton.Ch13       as P
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
    it "eval1" $ eval2 "1 + 3 + 2" `shouldBe` 6

    it "eval2" $ eval2 "1 + 3 + 2" `shouldBe` 6
    it "eval2" $ eval2 "1" `shouldBe` 1

    -- it "epxr" $ P.parse expr "1 - a ab" `shouldBe` [(Add (Val 1) (Val 3), "ab")]
    it "compile" $ evaluate (compile "1 + a") `shouldThrow` anyException
    it "compile" $ evaluate (eval2 "1 + a") `shouldThrow` anyException

    it "eval1 -" $ eval1 "3 - 2" `shouldBe` 1
    it "eval2 -" $ eval2 "3 - 2" `shouldBe` 1
    it "eval2 -" $ eval2 "1 + 3 - 2" `shouldBe` 2


