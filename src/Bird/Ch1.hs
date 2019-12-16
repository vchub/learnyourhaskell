{-# OPTIONS_GHC -Wall #-}
module Bird.Ch1 where

import           Control.Applicative
import           Data.Char
import           Data.List
import qualified Data.Map.Strict     as M
import qualified Data.Text           as T

count :: (Eq a) => a -> [a] -> Int
count a xs = sum [ 1 | x <- xs, x == a ]

squareCounter :: [String] -> M.Map String Int
squareCounter ss = go M.empty ss
 where
  go m []       = m
  go m (x : xs) = go (M.insertWith (+) x 1 m) xs

squareCount :: [String] -> [(String, Int)]
squareCount ss =
  sortBy (\(_, a) (_, b) -> compare b a) (M.toList (squareCounter ss))

countRuns :: T.Text -> [(T.Text, Int)]
-- countRuns::T.Text->[T.Text]
countRuns =
  (sortBy (\(_, x) (_, y) -> compare y x))
    . go
    . sort
    . (map T.toLower)
    . T.words
 where
  go []       = []
  go (x : xs) = (x, length l) : go r where (l, r) = break (x <) (x : xs)

numName :: M.Map Int String
numName = M.fromList
  [ (1 , "one")
  , (2 , "two")
  , (3 , "three")
  , (11, "eleven")
  , (12, "twelve")
  , (20, "twenty ")
  ]

fromPQ :: Int -> Int -> Maybe String
fromPQ n 100   = M.lookup n numName
fromPQ n 1000  = fmap (++ " hundred ") (M.lookup n numName)
fromPQ n 10000 = fmap (++ " thousand ") (M.lookup n numName)
fromPQ _ _     = error "fromPQ not implemented"
-- fromPQ p 20   = M.lookup p numName
-- fromPQ p 10  = M.lookup (10 + p) numName

sayNumber :: Int -> Maybe String
sayNumber x = go x 100 (Just "")
 where
  go :: Int -> Int -> Maybe String -> Maybe String
  go 0 _ acc = acc
  go n i acc
    | n < 20 = pure (++) <*> (fromPQ n i) <*> acc
    | n < 100 = foldl (liftA2 (++))
                      acc
                      [(fromPQ ((div n 10) * 10) 100), (fromPQ (mod n 10) i)]
    |
    -- TODO: that's wrong
      otherwise = go (div n 100)
                     (i * 10)
                     (pure (++) <*> (fromPQ (mod n i) i) <*> acc)


-- (pure (++) <*> (Just "some") <*> Just " foo")
-- a = Just 2
-- b = Just 2
-- pure (*) <*> a <*> b <*> a
-- foldl (liftA2 (+)) a [a,b,a]

{--
break (1<) [1,3,4]
break (==4) [1,3,4]
break (==4) [4]
break (==4) [4]
--}

-- ====================
-- Given a sorted array arr of distinct integers, return the lowest index i for
-- which arr[i] == i. Return null if there is no such index.
--
-- For example, given the array [-5, -3, 2, 3], return 2 since arr[2] == 2.
-- Even though arr[3] == 3, we return 2 since it's the lowest index.

arri :: [Int] -> Maybe Int
-- arri xs = go 0 (length xs) -- (length xs - 1)
arri xs = go 0 (length xs - 1)
 where
  go :: Int -> Int -> Maybe Int
  go l h | xs !! l > l  = Nothing
         | xs !! h < h  = Nothing
         | xs !! l == l = Just l
         | xs !! m >= m = go l m
         | otherwise    = go (m + 1) h
    where m = (l + h) `div` 2


-- | l >= h       = Nothing


{-
   Implement Int division
   div 10 3  = (3,1)
-}
div0 :: Int -> Int -> (Int, Int)
div0 a b | b == 0    = error "0 division"
         | otherwise = go 0
 where
  i = signum b
  go n | b * n > a = (n - i, a - (n - i) * b)
       | otherwise = go (n + i)

div1 :: Int -> Int -> (Int, Int)
div1 a b | b == 0          = error "0 division"
         | a >= 0 && b < 0 = (-1 * (q' + 1), b + r')
         | a < 0 && b > 0  = (-1 * (q' + 1), b - r')
         | a < 0 && b < 0  = (q', -1 * r')
         | otherwise       = (q', r')
 where
  go q n b' | n < b'    = (q, n)
            | {-{-   -}some commet -}
              otherwise = go (q + 1) (n - b') b'
  (q', r') = go 0 (abs a) (abs b)



units :: [String]
units =
  [ "zero"
  , "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]
teens :: [String]
teens =
  [ "ten"
  , "eleven"
  , "twelve"
  , "thirteen"
  , "fourteen"
  , "fifteen"
  , "sixteen"
  , "seventeen"
  , "eighteen"
  , "nineteen"
  ]
tens :: [String]
tens =
  ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

convert1 :: Int -> String
convert1 n = units !! (n)

convert :: Int -> String
convert 0  = "zero"
convert n' = unwords (go n')
 where
  go :: Int -> [String]
  go n
    | n == 0      = []
    | n < 10      = [units !! (n)]
    | n < 20      = [teens !! (n - 10)]
    | n < 100     = (tens !! (div n 10 - 2)) : (go (mod n 10))
    | n < 1000    = [(units !! (div n 100))] ++ ["hundred"] ++ (go (mod n 100))
    | n < 1000000 = [convert (div n 1000)] ++ ["thousand"] ++ (go (mod n 1000))
    | otherwise   = ["NULL"]


-- unwords ["one","two"]

flatten :: [[a]] -> [a]
flatten = foldl' (++) []


dictWords :: [String]
dictWords = ["ignore", "region", "ringer", "resign", "signer", "singer"]

makeDict :: [String] -> M.Map String [String]
makeDict = foldl' (\m w -> M.insertWith (++) (sort w) [w] m) M.empty

-- makeDict dictWords

cwords :: Int -> FilePath -> FilePath -> IO String
cwords n infile outfile = do
  text <- readFile infile
  putStrLn text
  writeFile outfile (take n text)
  putStrLn "cwords is done"
  return (take 4 text)

modernise :: String -> String
modernise = unwords . map capilize . words
 where
  capilize []       = []
  capilize (x : xs) = toUpper x : xs

myexp :: Int -> Int -> Int
myexp _ 0 = 1
myexp x 1 = x
myexp x n = y * y * z
 where
  y = myexp x (div n 2)
  z = myexp x (mod n 2)

myexp1 :: Int -> Int -> Int
myexp1 _ 0 = 1
myexp1 x 1 = x
myexp1 x n | odd n     = (myexp1 x (n - 1)) * x
           | otherwise = (myexp1 x (div n 2)) * (myexp1 x (div n 2))

ispalindrome :: String -> Bool
ispalindrome s = s' == reverse s'
  where s' = map toLower . filter isAlpha $ s



-- cwords 35 "./foo.txt" "./boo.txt"

-- if True then False else True
-- a = [(+), (-)]










