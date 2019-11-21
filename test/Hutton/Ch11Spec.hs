{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch11Spec where

import           Hutton.Ch11
import           Test.Hspec
-- import           Test.QuickCheck


spec :: Spec
spec = describe "Ch11" $ do
  -- describe "Exercise" $ do
    -- it "countNodes" $ countNodes (gametree empty O) `shouldBe` 549946
    -- it "maxDepth" $ maxDepth (gametree empty O) `shouldBe` 9

  describe "misc" $ do
    it "interleave" $ interleave '|' "abc" `shouldBe` "a|b|c"
    it "interleave" $ concat (interleave "|" ["abc","xx"]) `shouldBe` "abc|xx"

    let b1 = [[O, O, O], [O, X, O], [O, O, X]]
        b2 = [[X, B, B], [O, X, O], [O, O, X]]
     in do
      it "chop" $ chop 2 [1,2,3,4]  `shouldBe` ([[1,2],[3,4]]::[[Int]])
      it "chop" $ chop 3 [1,2,3,4]  `shouldBe` ([[1,2,3],[4]]::[[Int]])
      it "move" $ move b2 1 O  `shouldBe` [[[X, O, B], [O, X, O], [O, O, X]]]

      -- it "showGrid" $ showGrid b2 `shouldBe` "O | O | O"
      it "showRow" $ showRow (b1!!0) `shouldBe` "O | O | O"
      it "showRow" $ showRow (b2!!0) `shouldBe` "X |   |  "

      it "won b1" $ won b1 `shouldBe` True
      it "" $ wins O b1 `shouldBe` True
      it "" $ wins O b2 `shouldBe` False
      it "" $ wins X b2 `shouldBe` True
      it "" $ next O `shouldBe` X
      it "" $ diag b1 `shouldBe` [O, X, X]
      it ""
        $          diag [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        `shouldBe` ([1, 5, 9] :: [Int])
      it ""
        $          transpose' [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        `shouldBe` ([[1, 4, 7], [2, 5, 8], [3, 6, 9]] :: [[Int]])
