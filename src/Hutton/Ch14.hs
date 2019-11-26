{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch14 where

-- import           Data.Foldable
import           Data.Monoid

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Ord, Eq, Show, Read)

conj :: Ord a => Tree a -> a -> Tree a
conj Leaf x = Node Leaf x Leaf
conj (Node l v r) x | x < v     = Node (conj l x) v r
                    | x > v     = Node l v (conj r x)
                    | otherwise = Node l v r

fromList :: Ord a => [a] -> Tree a
fromList = foldr (flip conj) Leaf


instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Foldable Tree where
  -- fold Leaf         = mempty
  -- fold (Node l x r) = fold l <>  x <> fold r
  -- foldMap _ Leaf         = mempty
  -- foldMap g (Node l x r) = foldMap g l <> g x <> foldMap g r
  foldr _ i Leaf         = i
  foldr g i (Node l x r) = foldr g (g x (foldr g i r)) l

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf         = pure Leaf
  traverse g (Node l v r) = pure Node <*> traverse g l <*> g v <*> traverse g r


sum1 :: Num a => Tree a -> a
sum1 = getSum . foldMap Sum

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns


sqrtseq :: Float -> Float -> [Float]
-- sqrtseq y guess = xs where xs = guess : [ 1 / 2 * (x + y / x) | x <- xs ]
-- sqrtseq y guess = guess : [ 1 / 2 * (x + y / x) | x <- sqrtseq y guess ]
sqrtseq y guess = iterate (\x -> (x + y / x) / 2) guess


sqrt1 :: Float -> Float -> Float
sqrt1 y eps = go xs
 where
  xs = sqrtseq y 1
  go (x0 : x1 : rest) | abs (x0 - x1) < eps = x1
                      | otherwise           = go (x1 : rest)
  go _ = error "sqrt1 shouldn't be the case"


