{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch12Spec where

import           Hutton.Ch12
import           Test.Hspec
-- import           Test.QuickCheck


spec :: Spec
spec = describe "Ch12" $ do
  -- describe "Exercise" $ do
    -- let t:: Tree1 Char
    --     t= Node1 (Node1 (Leaf1 'a') (Leaf1 'b')) (Leaf1 'c') in do
    --       it "fmap" $ ftoUpper t `shouldBe` Node1 (Node1 (Leaf1 'A') (Leaf1 'B')) (Leaf1 'C')

  describe "Monad example" $ do
    let t:: Tree Char
        t= Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c') in do
          it "relabel" $ relabel t `shouldBe` Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
          it "relabel1" $ relabel1 t `shouldBe` Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
          it "alabel" $ fst (app (alabel t) 1) `shouldBe` Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
          it "mlabel" $ fst (app (mlabel t) 1) `shouldBe` Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
    it "powerset" $ powerset ([1,2]::[Int]) `shouldBe` [[1,2],[1],[2],[]]

  describe "Functor1" $ do
    it "fmap1 - map" $ fmap1 pred [1, 2] `shouldBe` ([0, 1] :: [Int])
    it "fmap1 Maybe1" $ fmap1 succ No `shouldBe` (No :: Maybe1 Int)
    it "fmap1 Maybe1" $ fmap1 succ (Jst 2) `shouldBe` (Jst 3 :: Maybe1 Int)

    it "fmap Maybe" $ inc (Just 2) `shouldBe` (Just 3 :: Maybe Int)
    it "fmap Maybe" $ inc (Jst 2) `shouldBe` (Jst 3 :: Maybe1 Int)
    it "fmap Maybe" $ inc [1 .. 4] `shouldBe` [2 .. 5]

    it "Maybe1 Applicative"
      $          pure (+)
      <*>        No
      <*>        Jst 1
      `shouldBe` (No :: Maybe1 Int)
    it "Maybe1 Applicative"
      $          pure (+)
      <*>        Jst 2
      <*>        Jst 1
      `shouldBe` (Jst 3 :: Maybe1 Int)
    it "Maybe1 Applicative"
      $          pure (+ 1)
      <*>        Jst 1
      `shouldBe` (Jst 2 :: Maybe1 Int)
    it "Maybe1 Applicative"
      $          Jst (+ 1)
      <*>        Jst 1
      `shouldBe` (Jst 2 :: Maybe1 Int)
    it "Maybe1 Applicative"
      $          Jst (+)
      <*>        Jst 1
      <*>        Jst 1
      `shouldBe` (Jst 2 :: Maybe1 Int)

    it "[] Applicative" $ pure (+ 1) <*> [1, 2] `shouldBe` ([2, 3] :: [Int])
    it "[] Applicative"
      $          pure (+)
      <*>        [1, 2]
      <*>        [3, 5]
      `shouldBe` ([4, 6, 5, 7] :: [Int])
    it "prods Applicative"
      $          prods [1, 2] [3, 4]
      `shouldBe` ([3, 4, 6, 8] :: [Int])










