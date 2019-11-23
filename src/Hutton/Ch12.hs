{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch12 where

import           Control.Monad
import           Data.Char

class Functor1 f where
  fmap1:: (a -> b)-> f a-> f b

instance Functor1 [] where
  fmap1 = map

data Maybe1 a = No | Jst a deriving (Show, Eq, Ord)

instance Functor1 Maybe1 where
  -- fmap1::(a->b)->f a -> f b
  fmap1 _ No      = No
  fmap1 g (Jst a) = Jst (g a)

instance Functor Maybe1 where
  -- fmap1::(a->b)->f a -> f b
  fmap _ No      = No
  fmap g (Jst a) = Jst (g a)

inc :: Functor f => f Int -> f Int
inc = fmap (+ 1)

instance Applicative Maybe1 where
  pure = Jst
  (<*>) No _       = No
  (<*>) (Jst g) mg = fmap1 g mg

prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure (*) <*> xs <*> ys

-- aAdd :: Applicative f => f Int -> f Int
-- aAdd = pure (+)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

relabel :: Tree Char -> Tree Int
relabel t = fst (go t [1 ..])
 where
  go (Leaf _  ) []       = error "algorithm is wrong"
  go (Leaf _  ) (n : ns) = ((Leaf n), ns)
  go (Node l r) ns       = (Node l' r', nsr)
   where
    (l', nsl) = go l ns
    (r', nsr) = go r nsl

relabel1 :: Tree Char -> Tree Int
relabel1 t = fst (go t 1)
 where
  go (Leaf _  ) n = ((Leaf n), (n + 1))
  go (Node l r) n = (Node l' r', n'')
   where
    (l', n' ) = go l n
    (r', n'') = go r n'

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  fmap g st = S(\s-> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  -- pure:: a -> ST a
  pure x = S(\s -> (x, s))
  -- (<*>)::ST(a->b)->ST a->ST b
  stf <*> stx = S(\s-> let (f,s')= app stf s
                           (x,s'') = app stx s'
                        in (f x, s''))

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _  ) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

instance Monad ST where
  -- (>>=) ST a -> (a->ST b) -> ST b
  st >>= f = S(\s-> let (x,s') = app st s in app (f x) s')


mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do
  n <- fresh
  return (Leaf n)
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')

powerset :: [a] -> [[a]]
powerset = filterM (\_ -> [True, False])

data Tree1 a = Leaf1 | Node1 (Tree1 a) a (Tree1 a) deriving (Eq, Show)

instance Functor Tree1 where
  fmap _ Leaf1         = Leaf1
  fmap f (Node1 l x r) = Node1 (fmap f l) (f x) (fmap f r)

ftoUpper :: Tree1 Char -> Tree1 Char
ftoUpper = fmap toUpper

-- instance Functor ((->) a) where
--   -- fmap:: (b->c) -> (a->b) -> (a->c)
--   fmap = (.)









