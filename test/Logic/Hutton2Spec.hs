module Logic.Hutton2Spec where

import           Data.Char
import           Data.List
-- import           Data.List.Ordered
import           Test.Hspec
import           Test.Hspec.QuickCheck
-- import qualified Test.QuickCheck.Gen           as Gen
-- import           Test.QuickCheck

evens :: [Int] -> [Int]
evens []       = []
evens (x : xs) = x : odds xs

odds :: [Int] -> [Int]
odds []       = []
odds (_ : xs) = evens xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort l) (msort r)
 where
  (l, r) = halve xs
  n      = div (length xs) 2
  halve xs = (take n xs, drop n xs)
  merge :: Ord a => [a] -> [a] -> [a]
  merge [] y  = y
  merge x  [] = x
  merge (x : xs) (y : ys) | x <= y    = x : merge xs (y : ys)
                          | otherwise = y : merge (x : xs) ys

twice :: (a -> a) -> a -> a
twice f a = f $ f a
-- twice (* 2) 2

map0 :: (a -> b) -> [a] -> [b]
map0 _ []       = []
map0 f (x : xs) = f x : map f xs

filter0 :: (a -> Bool) -> [a] -> [a]
filter0 _ [] = []
filter0 f (x : xs) | f x       = x : filter0 f xs
                   | otherwise = filter0 f xs

foldr0 :: (a -> b -> b) -> b -> [a] -> b
foldr0 _ i []       = i
foldr0 f i (x : xs) = f x (foldr0 f i xs)

reverse0 :: [a] -> [a]
reverse0 = foldr0 (\a b -> b ++ [a]) []

-- Binary string transmitter
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\a b -> b * 2 + a) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = mod n 2 : (int2bin $ div n 2)

make8 :: [Bit] -> [Bit]
make8 xs = take 8 (xs ++ (repeat 0))

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8
 where
  chop8 []   = []
  chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- map id ['a','b']

transmit :: String -> String
transmit = decode . channel . encode where channel = id

count :: Eq a => a -> [a] -> Int
-- count x = foldl' (\b a -> if a == x then b + 1 else b) 0
count x = length.filter (==x)

rmdups :: Eq a => [a] -> [a]
-- rmdups = foldl' (\b a -> if elem a b then b else a : b) []
rmdups []     = []
rmdups (x:xs) = x: filter (/=x) (rmdups xs)

result::Ord a=>[a]->[(Int,a)]
result xs = sort $ map (\x->(count x xs, x)) (rmdups xs)

winner::Ord a=>[a]->a
winner = snd.last.result

ballots ::   [[String]]
ballots = [["Red", "Green"],
  [],
  ["Blue"],
  ["Green", "Red", "Blue"],
  ["Blue", "Green", "Red"],
  ["Green"]]

rmempty::Eq a=> [[a]]->[[a]]
rmempty = filter (/=[])

elim::Eq a=> a->[[a]]->[[a]]
elim e = map (filter (/=e))

rank::Ord a=>[[a]]->[a]
rank = map snd . result . map head

winner'::Ord a=>[[a]]->a
winner' bs = case rank (rmempty bs) of
  [c]    -> c
  (c:cs) -> winner' (elim c bs)

flatten::[[a]]->[a]
flatten = foldr (++) []

altMap:: (a->b)->(a->b)->[a]->[b]
altMap fa fb xs = map (\(x,alt)-> if alt then fa x else fb x) (zip xs (flatten $ repeat [True,False]))


spec :: Spec
spec = describe "Hutton book" $ do

  describe "Exercise" $ do
    let xs = [1..4] in do
      it "" $ flatten [[1],[2,3]] `shouldBe` [1,2,3]
      it "" $ altMap (+1) (+10) xs `shouldBe` [2,12,4,14]

  describe "Alternative Voting" $ do
    let xs = [[1,2,3],[2,1],[3,1],[3,2],[1,3]] in do
      it "" $ elim 2 xs `shouldBe` [[1,3], [1],[3,1],[3],[1,3]]
      it "" $ rank xs `shouldBe` [2,1,3]
      it "" $ head (rank xs) `shouldBe` 2
      it "" $ winner' xs `shouldBe` 1
      it "" $ winner' ballots `shouldBe` "Green"

  describe "Voting" $ do
    let xs = [1, 2, 1, 0, 2] in do
      it "" $ count 1 xs `shouldBe` 2
      it "" $ count 1 [] `shouldBe` 0
      -- it "" $ rmdups xs `shouldBe` reverse [1,2,0]
      it "" $ rmdups xs `shouldBe` [1,2,0]
      it "" $ rmdups [] `shouldBe` ([]::[Int])
      it "result" $ result xs `shouldBe` [(1,0),(2,1),(2,2)]
      it "winner" $ winner xs `shouldBe` 2

  describe "Binary transmitter" $ do
    it "" $ decode (encode "abc") `shouldBe` "abc"
    it "" $ transmit "abc" `shouldBe` "abc"
    it ""
      $          transmit "higher-order functions are easy"
      `shouldBe` "higher-order functions are easy"

    it "" $ bin2int [] `shouldBe` 0
    it "" $ bin2int [1] `shouldBe` 1
    it "" $ bin2int [0, 1] `shouldBe` 2
    it "" $ bin2int [1, 0, 1] `shouldBe` 5
    it "" $ bin2int [1, 0, 1, 1] `shouldBe` 13
    it "" $ int2bin 0 `shouldBe` []
    it "" $ int2bin 1 `shouldBe` [1]
    it "" $ int2bin 2 `shouldBe` [0, 1]
    it "" $ int2bin 13 `shouldBe` [1, 0, 1, 1]
    it "" $ (make8 . int2bin) 13 `shouldBe` [1, 0, 1, 1, 0, 0, 0, 0]
    it ""
      $          encode "abc"
      `shouldBe` [ 1
                 , 0
                 , 0
                 , 0
                 , 0
                 , 1
                 , 1
                 , 0
                 , 0
                 , 1
                 , 0
                 , 0
                 , 0
                 , 1
                 , 1
                 , 0
                 , 1
                 , 1
                 , 0
                 , 0
                 , 0
                 , 1
                 , 1
                 , 0
                 ]

  describe "misc" $ do
    it "" $ twice reverse [1 .. 3] `shouldBe` [1 .. 3]
    it "" $ map0 (+ 1) [1 .. 3] `shouldBe` [2, 3, 4]
    it "" $ map0 (map0 (+ 1)) [[1, 2], [3, 4]] `shouldBe` [[2, 3], [4, 5]]
    it "" $ filter0 (> 2) [1 .. 5] `shouldBe` [3 .. 5]
    it "" $ filter0 (/= ' ') "ab cd" `shouldBe` "abcd"
    it "" $ foldr0 (+) 1 [] `shouldBe` foldr (+) 1 []
    it "" $ foldr0 (+) 1 [1 .. 3] `shouldBe` foldr (+) 1 [1 .. 3]
    it "" $ foldl' (flip (:)) [] [1 .. 3] `shouldBe` reverse [1 .. 3]
    it "" $ reverse0 [1 .. 4] `shouldBe` reverse [1 .. 4]

  describe "msort" $ do
    it "" $ msort [2, 1] `shouldBe` [1, 2]
    it "" $ msort [2, 1, 0] `shouldBe` [0, 1, 2]
    prop "sort msort" $ \xs -> msort xs `shouldBe` (sort xs :: [Int])

  describe "evens, odds" $ do
    it "" $ evens [1 .. 4] `shouldBe` [1, 3]
    it "" $ evens [1, 2] `shouldBe` [1]
    it "" $ evens [1] `shouldBe` [1]
    it "" $ odds [1 .. 4] `shouldBe` [2, 4]
