{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch13Spec where

import           Control.Applicative
import           Control.Exception
import           Data.Char
import           Hutton.Ch13
import           Test.Hspec
-- import           Test.QuickCheck


spec :: Spec
spec = describe "Ch13" $ do
  describe "Exercises" $ do
    -- it "comment" $ parse (takeWhileP (/= 'a')) "bdabc" `shouldBe` [("bd","abc")]
    it "comment" $ parse comment " -- foo \n abc" `shouldBe` [(()," abc")]

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

        it "empty" $ parse empty s `shouldBe` ([]::[(Char, String)])
        it "<|>" $ parse (item <|> empty) s `shouldBe` [('a', "bc")]
        it "<|>" $ parse (empty <|> item) s `shouldBe` [('a', "bc")]
        it "<|>" $ parse (empty <|> return 'd') "abcde"`shouldBe` [('d', "abcde")]

        it "sat" $ parse (sat (<='b')) s `shouldBe` [('a', "bc")]
        it "sat" $ parse (sat (>'c')) s `shouldBe` []

        it "digit" $ parse digit "1a" `shouldBe` [('1', "a")]
        it "digit" $ parse digit "a1" `shouldBe` []
        it "letter" $ parse letter "a1" `shouldBe` [('a', "1")]
        it "char" $ parse (char 'a') "a1" `shouldBe` [('a', "1")]
        it "char" $ parse (char 'b') "a1" `shouldBe` []
        it "string" $ parse (string "abc") "abcde" `shouldBe` [("abc", "de")]
        it "string" $ parse (string "abc") "ab123" `shouldBe` []

        it "many" $ parse (many digit) "ab123" `shouldBe` [("","ab123")]
        it "many" $ parse (many digit) "123a" `shouldBe` [("123","a")]
        it "many1" $ parse (many1 digit) "ab123" `shouldBe` [("","ab123")]
        it "many1" $ parse (many1 digit) "123a" `shouldBe` [("123","a")]

        it "some" $ parse (some digit) "ab123" `shouldBe` []
        it "some" $ parse (some digit) "123a" `shouldBe` [("123","a")]
        it "some1" $ parse (some1 digit) "ab123" `shouldBe` []
        it "some1" $ parse (some1 digit) "123a" `shouldBe` [("123","a")]

        it "ident" $ parse ident "123a" `shouldBe` []
        it "ident" $ parse ident "b123a c" `shouldBe` [("b123a"," c")]

        it "nat" $ parse nat "123 ac" `shouldBe` [(123, " ac")]
        it "nat" $ parse nat "123ac" `shouldBe` [(123,"ac")]
        it "nat" $ parse nat "x123ac" `shouldBe` []

        it "nat" $ parse space " ac" `shouldBe` [((), "ac")]
        it "nat" $ parse space "     ac" `shouldBe` [((), "ac")]

        it "int" $ parse int "-123 ab" `shouldBe` [((-123), " ab")]
        it "int" $ parse int "123 ab" `shouldBe` [(123, " ab")]

        it "identfier" $ parse identfier "  123a " `shouldBe` []
        it "identfier" $ parse identfier "  x123a c" `shouldBe` [("x123a", "c")]

        it "nats" $ parse nats " [1 , 23,4] ab" `shouldBe` [([1,23,4], "ab")]
        it "nats" $ parse nats " [1 , 23,4,] ab" `shouldBe` []

        it "epxr" $ parse expr "1 + 3 ab" `shouldBe` [(4, "ab")]
        it "epxr" $ parse expr "1 - 3 ab" `shouldBe` [((-2), "ab")]
        it "epxr" $ parse expr "1 / 3 ab" `shouldBe` [(0, "ab")]
        it "epxr" $ parse expr "5 / 3 ab" `shouldBe` [(1, "ab")]

        it "epxr" $ parse expr "1+3 ab" `shouldBe` [(4, "ab")]
        it "epxr" $ parse expr "4*(1+3) ab" `shouldBe` [(16, "ab")]
        it "epxr" $ parse expr "(2+1)*(1+3) ab" `shouldBe` [(12, "ab")]

        it "eval" $ eval "(2+1)*(1+3) " `shouldBe` 12
        it "eval" $ evaluate (eval "(2+1)*(1+3) ab") `shouldThrow` anyException
        it "eval" $ eval "(-2+8)/(5-3) " `shouldBe` 3




