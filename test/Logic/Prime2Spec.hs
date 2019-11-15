{-# OPTIONS_GHC -Wall #-}

module Logic.Prime2Spec where
import           Data.List
-- import           Data.Char
-- import           Data.List.Ordered
import           Test.Hspec
import           Test.Hspec.QuickCheck
-- import qualified Test.QuickCheck.Gen           as Gen
-- import           Test.QuickCheck


isPrime01 :: Integer -> Bool
isPrime01 n | n < 1     = error "not a positive Integer"
            | n == 1    = False
            | otherwise = ld 2 n == n
 where
  ld i n' | i * i > n     = n
          | mod n' i == 0 = i
          | otherwise     = ld (succ i) n

primes1 :: [Integer]
primes1 = 2 : (filter $ nondividedBy primes1) [3, 5 ..]
 where
  nondividedBy :: [Integer] -> Integer -> Bool
  nondividedBy (p : ps) n | p * p > n    = True
                          | mod n p == 0 = False
                          | otherwise    = nondividedBy ps n
  nondividedBy [] _ = False

rev0 :: [a] -> [a]
rev0 []       = []
rev0 (x : xs) = rev0 xs ++ [x]

rev :: [a] -> [a]
rev = foldl (flip (:)) []

maybeLast :: [a] -> Maybe a
maybeLast = foldl go Nothing
 where
  go Nothing a = Just a
  go _       a = Just a

fact0 :: Integer -> Integer
fact0 0 = 1
fact0 n = n * fact0 (n - 1)

concat0 :: [[a]] -> [a]
concat0 []         = []
concat0 (xs : xss) = xs ++ (concat0 xss)

qs :: Ord a => [a] -> [a]
qs []       = []
qs (x : xs) = (qs $ filter (<= x) xs) ++ [x] ++ (qs $ filter (> x) xs)

(!!!)::[a]->Int->Maybe a
[] !!! _ = Nothing
(x:_) !!! 0 = Just x
(_:xs) !!! n = xs !!! (n-1)

lookup1::Eq a => a->[(a,b)]->Maybe b
lookup1 x = foldr (\(a,b) acc -> if a==x then Just b else acc) Nothing

birthday::Float->Integer
birthday prob = go 1 1
  where
    go:: Integer->Float->Integer
    go n acc | 1-acc >= prob = n
             | otherwise = go (n+1) (acc * (fromInteger $ 365-n)/365 )

repeat1 :: a->[a]
repeat1 x = (x :repeat1 x)

blowup::String->String
blowup s = go "" 1 s
  where go:: String->Int->String->String
        go acc _ ""     = acc
        go acc i (x:xs) = go (acc ++ take i (repeat1 x)) (succ i) xs
        -- go acc i (x:xs) = go (acc ++ repeat0 i [x]) (succ i) xs
        -- repeat0 i x | i<1 = ""
        --             | otherwise = x ++ repeat0 (pred i) x

blowup2::String->String
blowup2 s = [x | (c,i) <- zip s [1,2..], x<- take i (repeat c)]

isprefix:: String->String->Bool
isprefix "" _          = True
isprefix (x:xs) (y:ys) = x == y && isprefix xs ys
isprefix _ _           = False

issubstr:: String->String->Bool
issubstr [] _   = True
issubstr _ []   = False
issubstr sub ss = isprefix sub ss || issubstr sub (drop 1 ss)

spec :: Spec
spec = describe "Discrete math" $ do
  describe "issubstr" $ do
    it "" $ issubstr "" "" `shouldBe` True
    it "" $ issubstr "a" "ba" `shouldBe` True
    it "" $ issubstr "ab" "abc" `shouldBe` True
    it "" $ issubstr "ab" "xabc" `shouldBe` True
    it "" $ issubstr "ab" "ccabc" `shouldBe` True
    it "" $ issubstr "ab" "xccabc" `shouldBe` True
    it "" $ issubstr "ab" "ccbac" `shouldBe` False

  describe "isprefix" $ do
    it "" $ isprefix "" "" `shouldBe` True
    it "a a" $ isprefix "a" "a" `shouldBe` True
    it "a " $ isprefix "a" "" `shouldBe` False
    it "_ a" $ isprefix "" "a" `shouldBe` True
    it "a ab" $ isprefix "a" "ab" `shouldBe` True
    it "ab abc" $ isprefix "ab" "abc" `shouldBe` True
    it "ab acc" $ isprefix "ab" "acc" `shouldBe` False

  describe "blowup2" $ do
    it "a" $ blowup2 "a" `shouldBe` "a"
    it "ab" $ blowup2 "ab" `shouldBe` "abb"
    it "abc" $ blowup2 "abc" `shouldBe` "abbccc"

  describe "blowup" $ do
    it "a" $ blowup "a" `shouldBe` "a"
    it "ab" $ blowup "ab" `shouldBe` "abb"
    it "abc" $ blowup "abc" `shouldBe` "abbccc"

  describe "birthday" $ do
    it "2" $ birthday (fromInteger 1- 364/365) `shouldBe` 2
    it "3" $ birthday (fromInteger 1- 364*363/(365*365)) `shouldBe` 4
    it "22" $ birthday 0.5 `shouldBe` 23

  describe "lookup1" $ do
    it "[]" $ lookup1 (1 ::Integer) [] `shouldBe` ( Nothing :: Maybe Integer)
    it "[]" $ lookup1 (2 ::Integer) [(1,2),(1,3),(2,4)] `shouldBe` ( Just 4 :: Maybe Integer)
    it "[]" $ lookup1 (1 ::Integer) [(1,2),(1,3),(2,4)] `shouldBe` ( Just 2 :: Maybe Integer)

  describe "!!!" $ do
    it "[]" $ [] !!! 1  `shouldBe` ( Nothing :: Maybe Integer)
    let xs = [1,2..10]::[Integer]
     in do
        it "1" $ xs !!! 0  `shouldBe` Just 1
        it "" $ xs !!! 1  `shouldBe` Just 2
        it "" $ xs !!! 2  `shouldBe` Just 3
        -- prop "prop" $ \i-> xs !!! i  `shouldBe` (Just (xs !! i)::Maybe Integer)


  describe "quic sort" $ do
    it "[]" $ qs ([] :: [Integer]) `shouldBe` []
    let x = [2,0,3,1,2]
        want = sort x ::[Integer]
      in it "x" $ qs x `shouldBe` want
    prop "sorted" $ \s-> qs s `shouldBe` sort (s::[Integer])


  describe "concat0" $ do
    it "[]" $ concat0 [] `shouldBe` ([] :: [Integer])
    it "[1]" $ concat0 [[1]] `shouldBe` ([1] :: [Integer])
    it "[[1]]" $ concat0 [[1], [2, 3]] `shouldBe` ([1, 2, 3] :: [Integer])

  describe "fact0" $ do
    it "0" $ fact0 0 `shouldBe` 1
    it "3" $ fact0 3 `shouldBe` 6

  describe "rev" $ do
    it "[1,2]" $ rev0 [1, 2] `shouldBe` ([2, 1] :: [Integer])
    it "[1,2]" $ rev [1, 2] `shouldBe` ([2, 1] :: [Integer])

  describe "maybeLast" $ do
    it "[1,2]" $ maybeLast [1, 2] `shouldBe` (Just 2 :: Maybe Integer)
    it "[]" $ maybeLast [] `shouldBe` (Nothing :: Maybe Integer)

  describe "dummy" $ do
    it "+" $ foldl (+) 0 [1, 2] `shouldBe` (3 :: Int)

  describe "isPrime0" $ do
    it "2,3,5,7,11,13" $ all isPrime01 [2, 3, 5, 7, 11, 13] `shouldBe` True
    it "4,6,9" $ all (not . isPrime01) [4, 6, 9] `shouldBe` True

  describe "isPrime0" $ do
    it "first primes1" $ take 5 primes1 `shouldBe` [2, 3, 5, 7, 11]
    it "first primes1" $ all isPrime01 (take 5 primes1) `shouldBe` True
