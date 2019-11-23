{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch13Spec where

import           Data.Char
import           Hutton.Ch13
import           Test.Hspec
-- import           Test.QuickCheck


spec :: Spec
spec = describe "Ch13" $ do
  describe "Parser" $ do
    let s = "abc"
     in do
        it "item" $ parse item s `shouldBe` [('a', "bc")]
        it "fmap" $ parse (fmap toUpper item) s `shouldBe` [('A', "bc")]
        it "fmap _" $ parse (fmap toUpper item) "" `shouldBe` []
        it "pure" $ parse (pure (1::Int)) s`shouldBe` [(1,s)]
        it "<*>" $ parse ((pure toUpper) <*> item ) s`shouldBe` [('A', "bc")]
        it "three" $ parse three s`shouldBe` [(('a','c'), "")]
        it "three []" $ parse three "ab" `shouldBe` []
        it "three1" $ parse three1 s`shouldBe` [(('a','c'), "")]
        it "three1 []" $ parse three1 "ab" `shouldBe` []
        it "three1" $ parse three1 "abcde"`shouldBe` [(('a','c'), "de")]
