{-# OPTIONS_GHC -Wall #-}

module Logic.Prime2Spec where
import           Data.Char
import           Data.List
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
primes1 = 2 : filter (nondividedBy primes1) [3, 5 ..]
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


takeWhile1::(a->Bool)->[a]->[a]
takeWhile1 _ [] = []
takeWhile1 f (x:xs) | f x = (x:takeWhile1 f xs)
                    | otherwise = []

{-
   [1,2]<[3,4]


 -}

-- intSqrt :: Int -> Int
-- intSqrt x | x<0 = error "negative intSqrt"
--           | otherwise = floor.sqrt.fromInteger $ x

-- primes3:: [Integer]
-- -- primes3 = [x | x<-(2:[3,5..]), all (\a-> (mod x a) /= 0 ) (takeWhile1 (\a-> a*a < x) primes3) ]
-- primes3 = [x | x<-(2:[3,5..]), all (\a-> (mod x a) /= 0 ) (takeWhile1 (\a-> a*a < 10) primes3) ]

fibs::[Integer]
fibs = [1,1] ++ [x | x<- zipWith (+) fibs (drop 1 fibs)]

and'::Bool->Bool->Bool
and' True b  = b
and' False _ = False

mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z”
mult = \x->(\y->(\z->x*y*z))

makeList::Integer->[Integer]
makeList n' = go n' []
  where go 0 acc = acc
        go n acc = go (div n 10)  (mod n 10 : acc)

luhnAlgo::Integer->Bool
luhnAlgo n' = (sum $ go n' False []) `mod` 10 == 0
  where go 0 _ acc     = acc
        go n isOdd acc = go (div n 10) (not isOdd)  (transf n isOdd : acc)
        transf n True  = if 2*n > 9 then 2*n-9 else 2*n
        transf n False = n

concat1::[[a]]->[a]
concat1 xss = [x | xs<-xss, x<-xs]

find1::Eq a=> a -> [(a,b)] -> [b]
find1 k' kvs = [v | (k,v)<-kvs, k==k']

pairs:: [a]->[(a,a)]
pairs xs = zip xs (tail xs)

char2Int::Char->Int
char2Int c = ord c - ord 'a'

int2Char::Int->Char
int2Char n = chr $ ord 'a' + n

shift::Int->Char->Char
shift n c | isLower c= int2Char $ mod (n + char2Int c) 26
        | otherwise = c

encode::Int->String->String
encode n ss = [shift n c | c<-ss]

-- frequencies of English letters
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

count::Eq a => a -> [a] -> Int
count c cs = sum [1 | x<- cs, x == c]

str2Ocur:: String->[Int]
str2Ocur cs = [count c cs | c<-['a'..'z']]

toFreq:: [Int]->[Float]
toFreq xs = [fromIntegral x/n | x<-xs]
  where n = fromIntegral (length xs)

min1::Ord a => [(a, Int)]->(a, Int)
min1 []     = error "min1 of empty list"
min1 (x:xs) = foldl' (\a b -> if fst a < fst b then a else b) x xs

rotate::[a]->Int->[a]
rotate s i = (drop i s) ++ (take i s)

diff::Num a => [a]->[a]->[a]
diff x y = zipWith (-) x y

-- type Norm = [a]->[a]->a

norm::Num a => [a]->a
norm xs = sum [x*x | x <- xs]

diffNorm::Num a => [a]->[a]->a
diffNorm x y = norm $ diff x y

chiNorm::Fractional a => [a]->[a]->a
chiNorm x y = sum $ zipWith (\a b-> (a-b)*(a-b)/a) x y


findMinShift::(Ord a, Fractional a) => [a]->[a]->Int
findMinShift tbl sample = snd $ min1 $ [((chiNorm tbl (rotate sample i)), i) |
          i<-[0..length sample]]

hack::String -> String
hack s = encode (negate m) s
  where m = findMinShift table sample
        sample = toFreq $ str2Ocur s


spec :: Spec
spec = describe "Discrete math" $ do

  describe "hack" $ do
    it "bb -> ee" $ hack "bb" `shouldBe` "ee"
    it "zdd -> aee" $ hack "zdd" `shouldBe` "aee"
    it "kdvnhoo lv ixq" $ hack "kdvnhoo lv ixq" `shouldBe` "haskell is fun"
    it "vscd mywzboroxcsyxc kbo ecopev" $ hack "vscd mywzboroxcsyxc kbo ecopev"
        `shouldBe` "list comprehensions are useful"



  describe "frequencies" $ do
    it "min1" $ min1 ([(0.5, 1), (0.4, 2)]::[(Float, Int)]) `shouldBe` (0.4, 2)
    let x = ([1,2,3] :: [Int])
        y = ([0,2,1] :: [Int])
     in do
      it "norm of x - y" $ norm (diff x y) `shouldBe` 5
      it "rotate" $ rotate x 0 `shouldBe` x
      it "rotate" $ rotate x 1 `shouldBe` [2,3,1]
      it "rotate" $ rotate x 2 `shouldBe` [3,1,2]

    it "count" $ count 'x' "some" `shouldBe` 0
    it "count" $ count 'x' "soxmxe" `shouldBe` 2
    let ss = "aba"
        fr = str2Ocur ss
        want = take 2 fr
     in do
      it "str2Ocur" $ want `shouldBe` [2,1]
      it "str2Ocur" $ all (== 0) (drop 2 fr) `shouldBe` True

  describe "encode" $ do
    it "" $ encode (-5) (encode 5 "xyz")  `shouldBe` "xyz"
    it "" $ encode (-10) (encode 10 ['a'..'z'])  `shouldBe` ['a'..'z']
    it "" $ encode 3 "haskell is fun"  `shouldBe` "kdvnhoo lv ixq"
    it "" $ encode (-3)  "kdvnhoo lv ixq"`shouldBe` "haskell is fun"

  describe "mix" $ do
    let sorted::[Int]->Bool
        -- sorted xs = all (\(a,b)-> a<=b) (pairs xs)
        sorted xs = and [x<=y | (x,y)<- pairs xs]

        positons:: Int->[Int]->[Int]
        positons x xs = [i | (i,y)<-zip [0..] xs, y==x]
     in do
      it "" $ sorted [1,2,3,4] `shouldBe` True
      it "" $ sorted [1,5,3,4] `shouldBe` False
      it "" $ positons 1 [1,5,1,4] `shouldBe` [0,2]
      it "" $ positons 5 [1,5,1,4] `shouldBe` [1]
      it "" $ positons 6 [1,5,1,4] `shouldBe` []


  describe "pairs" $ do
    it "" $ pairs ([1,2,3,4]::[Int]) `shouldBe` [(1,2),(2,3),(3,4)]

  describe "find1" $ do
    it "[(1,2)]" $ find1 1 ([(1,2)]::[(Int, Int)])  `shouldBe` [2]
    it "[(1,2)]" $ find1 1 ([(2,3),(1,2)]::[(Int, Int)])  `shouldBe` [2]
    it "[(1,2)]" $ find1 1 ([(2,3),(1,2),(1,4)]::[(Int, Int)])  `shouldBe` [2,4]

  describe "concat1" $ do
    it "1" $ concat1 ([[1,2],[3,4]]::[[Int]]) `shouldBe` [1..4]
    let xss = ([[1..x] | x<-[1..10]]::[[Int]])
     in do
       it "xss" $ concat1 xss `shouldBe` concat xss

  describe "luhnAlgo" $ do
    it "makeList" $ makeList 2 `shouldBe` [2]
    it "makeList" $ makeList 29 `shouldBe` [2,9]
    it "makeList" $ makeList 729 `shouldBe` [7,2,9]
    it "makeList" $ makeList 9929 `shouldBe` [9,9,2,9]
    it "luhnAlgo 9" $ luhnAlgo 9 `shouldBe` False
    it "luhnAlgo" $ luhnAlgo 99 `shouldBe` False
    it "luhnAlgo" $ luhnAlgo 1784 `shouldBe` True
    it "luhnAlgo" $ luhnAlgo 4783 `shouldBe` False
    it "luhnAlgo" $ luhnAlgo 79927398713 `shouldBe` False
    -- it "luhnAlgo" $ luhnAlgo 4024 `shouldBe` True
    -- prop "all < 10" $ \n -> all (<10) (makeList n) == True

  describe "mult" $ do
    it "" $ mult 1 2 3 `shouldBe` 6
    prop "mult" $ \x y z -> mult x y z `shouldBe` x*y*z

  describe "and'" $ do
    it "" $ and' True False `shouldBe` False
    prop "and" $ \a b -> and' a b `shouldBe` a && b

  describe "fibs" $ do it "1,1,2.." $ take 8 fibs `shouldBe` [1,1,2,3,5,8,13,21]

  describe "takeWhile1" $ do
    it "odd" $ takeWhile1 odd ([]::[Integer]) `shouldBe` []
    it "odd" $ takeWhile1 odd ([1,3,5,6]::[Integer]) `shouldBe` [1,3,5]
    it "odd" $ takeWhile1 odd ([1,4,5,6]::[Integer]) `shouldBe` [1]
    it "odd" $ takeWhile1 (\x-> x*x < 5) ([1,2..]::[Integer]) `shouldBe` [1,2]

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

  describe "primes1" $ do
    it "first primes1" $ take 5 primes1 `shouldBe` [2, 3, 5, 7, 11]
    it "first primes1" $ all isPrime01 (take 5 primes1) `shouldBe` True
