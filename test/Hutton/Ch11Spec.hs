{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch11Spec where

import           Hutton.Ch11
import           Test.Hspec
-- import           Test.QuickCheck


spec :: Spec
spec = describe "Ch11" $ do

  describe "misc" $ do
    it "" $ next O `shouldBe` X
    it "" $ diag [[O, O, O], [O, X, O], [O, O, X]] `shouldBe` [O, X, X]
    it ""
      $          diag [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
      `shouldBe` ([1, 5, 9] :: [Int])
    it ""
      $          transpose' [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
      `shouldBe` ([[1, 4, 7], [2, 5, 8], [3, 6, 9]] :: [[Int]])
