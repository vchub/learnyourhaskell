{-# OPTIONS_GHC -Wall #-}
module D1Spec where
import           Test.Hspec


max' :: (Ord a) => a -> a -> a
max' a b | a > b     = a
         | otherwise = b

length' :: (Num a) => [t] -> a
length' xs = sum [ 1 | _ <- xs ]

maxXs :: (Ord a) => [a] -> a
maxXs []           = error "Can't handle empty list"
maxXs [x]          = x
maxXs [x, y]       = max' x y
maxXs (x : y : xs) = max' (max' x y) (maxXs xs)

spec :: Spec
spec = describe "Misc" $ do
  describe "lists" $ do
    it "++" $ [1, 2] ++ [3] `shouldBe` ([1, 2, 3] :: [Int])

  describe "max'" $ do
    it "2 3.4" $ max' 2 3.4 `shouldBe` (3.4 :: Double)
    it "2 3" $ max' 2 3 `shouldBe` (3 :: Int)
    it "5 3" $ max' 5 3 `shouldBe` (5 :: Int)

  -- describe "maxXs'" $ do
  --   it "[2]" $ maxXs [2] `shouldBe` 2
  --   it "[2 3]" $ maxXs [2, 3] `shouldBe` 3
  --   it "[2 3 1]" $ maxXs [2, 3, 1] `shouldBe` 3
  --   it "[2 3 6]" $ maxXs [2, 3, 6] `shouldBe` 6


foo :: IO ()
foo = do
  putStrLn "hello vlad"
  putStrLn "another hi ...."
  hspec spec

-- [x^2 | x<-[1..8], odd x]
