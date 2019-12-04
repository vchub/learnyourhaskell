{-# OPTIONS_GHC -Wall #-}
module Bird.Ch1Spec where

-- import           Data.List     (sortBy)
import           Bird.Ch1
import qualified Data.Text  as T
-- import           Data.List.Split
-- import qualified Data.Map   as M
import           Test.Hspec

spec :: Spec
spec = describe "Ch1" $ do
  describe "countRuns" $ do
    let s =T.pack  "t a b a t"
     in do
      it "countRuns" $ countRuns s `shouldBe` [(T.pack "a",2),(T.pack "t",2),(T.pack "b",1)]

  describe "squareCount" $ do
    it "count" $ count 1 ([2,3,1,1,2]::[Int]) `shouldBe` 2
    let s =words  "t a b a t"
     in do
      -- it "squareCount" $ M.toList (squareCount s) `shouldBe` [("a", 2)]
      it "squareCount" $ squareCount s `shouldBe` [("a", 2),("t", 2),("b", 1)]

  describe "sayNumber" $ do
    it "fromPQ" $ fromPQ 1 100  `shouldBe` Just "one"
    it "eleven" $ fromPQ 11 100  `shouldBe` Just "eleven"
    it "fromPQ" $ fromPQ 3 1000  `shouldBe` Just "three hundred "
    it "sayNumber" $ sayNumber 1  `shouldBe` Just "one"
    it "sayNumber 11" $ sayNumber 11  `shouldBe` Just "eleven"
    it "sayNumber 21" $ sayNumber 21  `shouldBe` Just "twenty one"
    it "sayNumber 101" $ sayNumber 101  `shouldBe` Just "one hundred one"













