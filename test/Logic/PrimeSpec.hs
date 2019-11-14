{-# OPTIONS_GHC -Wall #-}

module Logic.PrimeSpec where
import           Data.Char
-- import           Data.List.Ordered
import           Test.Hspec
import           Test.Hspec.QuickCheck
-- import qualified Test.QuickCheck.Gen           as Gen
import           Test.QuickCheck

isPrime0 :: Integer -> Bool
isPrime0 n | n < 1     = error "not a positive Integer"
           | n == 1    = False
           | otherwise = ldf 2 n == n
 where
  ldf i n' | i ^ (2 :: Integer) > n' = n'
           | mod n' i == 0           = i
           | otherwise               = ldf (succ i) n'


spec :: Spec
spec = describe "Primes and cypher" $ do
  describe "isPrime0" $ do
    it "+" $ foldl (+) 0 [1, 2] `shouldBe` (3 :: Int)
    it "[2,3,5,7,11]" $ all isPrime0 [2, 3, 5, 7, 11] `shouldBe` True
    it "[2,3,5,7,11,12]" $ all isPrime0 [2, 3, 5, 7, 11, 12] `shouldBe` False
    it "[10,12]" $ all (not . isPrime0) [10, 12] `shouldBe` True

  describe " Caesar cypher and assoiativity of function composition"
    $ let shift      = (+ 3)
          encodeChar = chr . shift . ord
          encodeStr  = map encodeChar
      in  do
            it "ab -> de" $ encodeStr "ab" `shouldBe` "de"
            prop "assoiativity of function composition f (g h)"
              $ \s -> map (chr . (shift . ord)) s `shouldBe` encodeStr s
            prop "assoiativity of function composition (f g)(h)" $ \s ->
              map (\c -> (chr . shift) $ ord c) s `shouldBe` encodeStr s

  describe "prime1" $ do
    it "divides 2 3" $ divides 2 3 `shouldBe` False
    it "divides 3 9" $ divides 3 9 `shouldBe` True
    it "primes1"
      $          take 11 primes1
      `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]

  describe "primeFactors" $ do
    it "[2,3,4,6]"
      $          map primeFactors [2, 3, 4, 6, 8]
      `shouldBe` [[2], [3], [2, 2], [2, 3], [2, 2, 2]]
    it "n = product factors" $ (product $ primeFactors 10) `shouldBe` 10
    it "n = product factors 1003"
      $          (product $ primeFactors 1003)
      `shouldBe` 1003
    it "n = product factors property" $ prop_Product0
    it "n = product factors property 2" $ forAll (choose (1, 100)) $ \n ->
      let factors = primeFactors n
      in  product factors == n && all isPrime1 factors

  describe "property" $ do
    it "reverse" $ property $ prop_reverse
    prop "reverse 2" $ prop_reverse

  describe "isPrime1" $ do
    it "1" $ isPrime1 1 `shouldBe` False
    it "2" $ isPrime1 2 `shouldBe` True
    it "11" $ isPrime1 11 `shouldBe` True
    it "= isPrime0" $ forAll (choose (1, 1000)) $ \n -> isPrime1 n == isPrime0 n

divides :: Integer -> Integer -> Bool
divides a n = mod n a == 0

primes1 :: [Integer]
primes1 = 2 : (filter $ nondividedBy primes1) [3, 5 ..]
 where
  nondividedBy :: [Integer] -> Integer -> Bool
  nondividedBy (p : ps) n | p * p > n   = True
                          | divides p n = False
                          | otherwise   = nondividedBy ps n
  nondividedBy [] _ = False

isPrime1 :: Integer -> Bool
isPrime1 n | n <= 1    = False
           | otherwise = find primes1
 where
  find (p : ps) | n < p     = False
                | n == p    = True
                | otherwise = find ps
  find [] = False

primeFactors :: Integer -> [Integer]
primeFactors n' = f n' primes1
 where
  f :: Integer -> [Integer] -> [Integer]
  f n ps@(p : rest) | p > n       = []
                    | divides p n = p : f (div n p) ps
                    | otherwise   = f n rest
  f _ [] = []

genNat :: Gen Integer
genNat = choose (1, 100)

prop_Product0 :: Property
prop_Product0 =
  forAll (choose (1, 1000) :: Gen Integer) $ \i -> product (primeFactors i) == i

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

main :: IO ()
main = do
  s <- getLine
  putStrLn $ "hi there " ++ s

{-
   :{
   s = putStrLn "Hi there!"

   :}
-}

