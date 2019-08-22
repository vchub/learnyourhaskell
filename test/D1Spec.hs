{-# OPTIONS_GHC -Wall #-}
module D1Spec where
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Data.List
-- import           CodeWorld

max' :: (Ord a) => a -> a -> a
max' a b | a > b     = a
         | otherwise = b

length' :: (Num a) => [t] -> a
length' xs = sum [ 1 | _ <- xs ]

maxXs :: (Ord a) => [a] -> a
maxXs []           = error "Can't handle empty list"
maxXs [x]          = x
maxXs [x, y]       = max' x y
maxXs (x : y : xs) = max (max x y) (maxXs xs)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
-- quickSort [x] = [x]
quickSort (x : xs) =
  let smaller = quickSort (filter (<= x) xs)
      bigger  = quickSort (filter (> x) xs)
                                                              -- let smaller = quickSort [ y | y <- xs, y <= x ]
                                                                  -- bigger  = quickSort [ y | y <- xs, y > x ]
  in  smaller ++ [x] ++ bigger


spec :: Spec
spec = describe "Misc" $ do
  describe "quickSort" $ do
    it "empty" $ quickSort [] `shouldBe` ([] :: [Int])
    it "one" $ quickSort [1] `shouldBe` ([1] :: [Int])
    it "3,2" $ quickSort [3, 2] `shouldBe` ([2, 3] :: [Int])
    prop "sort prop" $ \list -> quickSort list `shouldBe` sort (list :: [Int])

  describe "lists" $ do
    it "++" $ [1, 2] ++ [3] `shouldBe` ([1, 2, 3] :: [Int])

  describe "max'" $ do
    it "2 3.4" $ max' 2 3.4 `shouldBe` (3.4 :: Double)
    it "2 3" $ max' 2 3 `shouldBe` (3 :: Int)
    it "5 3" $ max' 5 3 `shouldBe` (5 :: Int)

  describe "maxXs'" $ do
    -- evaluate (error "foo") `shouldThrow` errorCall "foo"
    -- it "[]" $ maxXs ([] :: [Int]) `shouldThrow` anyException
    -- it "[]" $ maxXs ([] :: [Int]) `shouldThrow` anyErrorCall
    -- it "[]" $ maxXs ([] :: [Int]) `shouldBe` 1
    it "[2]" $ maxXs [2] `shouldBe` (2 :: Int)
    it "[2 3 1]" $ maxXs [2, 3, 1] `shouldBe` (3 :: Int)
    it "[2 3 6]" $ maxXs [2, 3, 6] `shouldBe` (6 :: Int)


foo :: IO ()
foo = do
  putStrLn "hello vlad"
  putStrLn "another hi ...."
  hspec spec

-- [x^2 | x<-[1..8], odd x]
-- Comment

-- lst = [1, 2, 3]
-- sum(map (2*) lst)

