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


  describe "arri problem" $ do
    it "arri" $  arri [0]  `shouldBe` Just 0
    it "arri" $  arri [0, 1]  `shouldBe` Just 0
    it "[-1, 1]" $  arri [-1, 1]  `shouldBe` Just 1
    it "[-1, 1, 2, 3]" $  arri [-1, 1, 2, 3]  `shouldBe` Just 1
    it "arri" $  arri [-1, 0, 2]  `shouldBe` Just 2
    it "arri" $  arri [-1, 0, 3]  `shouldBe` Nothing
    it "arri" $  arri [-1, 0, 1, 3]  `shouldBe` Just 3
    it "[-1, 0, 3, 3, 4]" $  arri [-1, 0, 3, 3, 4]  `shouldBe` Nothing

  describe "div0" $ do
    it "3 1" $  div0 3 1 `shouldBe` (3,0)
    it "10 3" $  div0 10 3 `shouldBe` (3,1)
    it "0 3" $  div0 0 3 `shouldBe` (0,0)
    -- it "10 -3" $  div0 10 (-3) `shouldBe` (-4,-2)
    -- it "-10 -3" $  div0 (-10) (-3) `shouldBe` (3,1)


  describe "div1" $ do
    it "3 1" $  div1 3 1 `shouldBe` (3,0)
    it "10 3" $  div1 10 3 `shouldBe` (3,1)
    it "0 3" $  div1 0 3 `shouldBe` (0,0)
    it "-10 -3" $  div1 (-10) (-3) `shouldBe` (3,-1)
    it "10 -3" $  div1 10 (-3) `shouldBe` (-4,-2)
    it "-10 3" $  div1 (-10) 3 `shouldBe` (-4,2)

  describe "convert1, 2" $ do
    it "convert1" $ convert1 0 `shouldBe` "zero"
    it "convert1" $ convert1 1 `shouldBe` "one"
    it "convert" $ convert 1 `shouldBe` "one"
    it "convert" $ convert 11 `shouldBe` "eleven"
    it "convert" $ convert 10 `shouldBe` "ten"
    it "convert" $ convert 19 `shouldBe` "nineteen"
    it "convert" $ convert 20 `shouldBe` "twenty"
    it "convert" $ convert 31 `shouldBe` "thirty one"
    it "convert" $ convert 99 `shouldBe` "ninety nine"
    it "convert" $ convert 101 `shouldBe` "one hundred one"
    it "convert" $ convert 111 `shouldBe` "one hundred eleven"
    it "convert" $ convert 921 `shouldBe` "nine hundred twenty one"
    it "convert" $ convert 1921 `shouldBe` "one thousand nine hundred twenty one"
    it "convert" $ convert 11921 `shouldBe` "eleven thousand nine hundred twenty one"

  describe "flatten" $ do
    it "flatten" $ flatten [[1],[]] `shouldBe` ([1]::[Int])
    it "flatten" $ flatten [[1],[2,3]] `shouldBe` ([1,2,3]::[Int])
    it "flatten" $ unwords (flatten [["some"],["foo", "bar"]]) `shouldBe` "some foo bar"

  describe "ch2 exercises" $ do
    it "modernise" $ modernise "" `shouldBe` ""
    it "modernise" $ modernise "some foo" `shouldBe` "Some Foo"
    it "myexp" $ myexp 2 0 `shouldBe` 1
    it "myexp" $ myexp 2 1 `shouldBe` 2
    it "myexp" $ myexp 2 3 `shouldBe` 8
    it "myexp" $ myexp 2 4 `shouldBe` 16
    it "myexp" $ myexp1 2 4 `shouldBe` 16
    -- it "myexp" $ map (\x-> myexp 2 x) [0..4]`shouldBe` map (2^) [0..4]
    -- it "myexp" $ map (\x-> myexp1 2 x) [0..4]`shouldBe` map (2^) [0..4]
    it "ispalindrome" $ ispalindrome "" `shouldBe` True
    it "ispalindrome" $ ispalindrome "eme" `shouldBe` True
    it "ispalindrome" $ ispalindrome "emE " `shouldBe` True
    it "ispalindrome" $ ispalindrome "e\n mE " `shouldBe` True

  -- describe "cwords" $ do
  --   got <- (cwords 5 "./foo.txt" "./boo.txt")
  --   it "cwords" $ (read got) `shouldBe` "some"





