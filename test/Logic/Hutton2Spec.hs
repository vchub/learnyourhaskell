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

data Nat = Zero | Succ Nat deriving (Show)

nat2int::Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1+nat2int n

int2nat::Int-> Nat
int2nat n | n == 0 = Zero
    | n < 0 = error "negative int to Nat"
    | otherwise = Succ (int2nat (n-1))

add::Nat->Nat->Nat
add Zero n     = n
add (Succ n) y = Succ (add n y)

mult::Nat->Nat->Nat
mult Zero n          = Zero
mult (Succ (Zero)) y = y
mult (Succ n) y      = add (mult n y) y


-- ====================
data Tree a = Leaf | Node (Tree a) a (Tree a)

conj:: Ord a=> Tree a ->a -> Tree a
conj Leaf a = Node Leaf a Leaf
conj (Node l b r) a | a <= b = Node (conj l a) b  r
      | otherwise = Node l b (conj r a)

lst2tree::Ord a => [a]->Tree a
lst2tree = foldl' conj Leaf

tree2lst::Ord a => Tree a->[a]
tree2lst Leaf         = []
tree2lst (Node l a r) = (tree2lst l) ++ [a]++ (tree2lst r)

occurs::Ord a=> a->Tree a->Bool
occurs _ Leaf = False
occurs x (Node l a r) | x<a = occurs x l
        | x>a = occurs x r
      | otherwise  = True

balanced::Ord a=> Tree a->Bool
balanced Leaf         = True
balanced (Node l _ r) = abs ((treeHeight l) -(treeHeight r)) <= 1 && (balanced l) && (balanced r)

treeHeight::Tree a->Int
treeHeight Leaf         = 0
treeHeight (Node l _ r) = (max (treeHeight l) (treeHeight r)) + 1

balance::Ord a=> [a]->Tree a
balance [] = Leaf
balance (x:xs) = Node (balance l) x (balance r)
  where n = length xs `div` 2
        l = take n xs
        r = drop n xs

-- Propositions and tautology
-- ====================

type Assoc k v = [(k,v)]

findK::Eq k => k-> Assoc k v  -> v
findK x d = head [v | (k,v)<-d, k==x]

data Prop = Const Bool
        | Var Char
        | Not Prop
        | And Prop Prop
        | Imply Prop Prop deriving (Show)

type Subst = Assoc Char Bool

eval0:: Subst->Prop->Bool
eval0 _ (Const c)     = c
eval0 tbl (Var x)     = findK x tbl
eval0 tbl (Not a)     = not (eval0 tbl a)
eval0 tbl (And a b)   = eval0 tbl a && eval0 tbl b
-- eval0 tbl (Imply a b) = not (eval0 tbl a) || (eval0 tbl b)
eval0 tbl (Imply a b) = eval0 tbl a <= eval0 tbl b

vars::Prop->[Char]
vars = rmdups.go
 where
    go (Const x)   = []
    go (Var x)     = [x]
    go (Not a)     = go a
    go (And a b)   = go a ++ go b
    go (Imply a b) = go a ++ go b

bools::Prop->[Bool]
bools p = map (\tbl -> eval0 tbl p) tbls
  where
        tmp = [(var, val) | val<-[False, True], var<-(vars p)]
        tbls = makechunks (length (vars p)) tmp
        -- makechunks _ [] = []
        makechunks n xs | length xs <= n = [xs]
        makechunks n xs = take n xs : makechunks n (drop 1 xs)

tautology::Prop->Bool
tautology p = and (bools p)

-- ====================
-- Abstract machine - Add Expression

data Expr = Val Int | Add Expr Expr | Mult Expr Expr

value::Expr -> Int
-- value (Val x)   = x
-- value (Add x y) = value x + value y
value e = eval e []

type Cont = [Op]
data Op = EVAL Expr | ADD Int | MULT Int

eval::Expr->Cont->Int
eval (Val n) c    = exec c n
eval (Add x y) c  = eval x (EVAL y:c)
eval (Mult x y) c = eval x (EVAL y:c)

exec::Cont->Int->Int
exec [] n          = n
exec (EVAL e :c) n = eval e (ADD n : c)
exec (ADD n :c) m  = exec c (n+m)
-- exec (MULT n :c) m = exec c (n*m)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n)   = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

evale::Expr -> Int
evale e = folde id (+) e

size::Expr -> Int
size e = folde (const 1) (+) e

spec :: Spec
spec = describe "Hutton book" $ do

  describe "Expr" $ do
    it "" $ value (Add (Add (Val 2) (Val 3)) (Val 4)) `shouldBe` 9
    it "" $ value (Add (Add (Val (-2)) (Val 3)) (Val 4)) `shouldBe` 5
    it "" $ evale (Add (Add (Val 2) (Val 3)) (Val 4)) `shouldBe` 9
    it "" $ size (Add (Add (Val 2) (Val 3)) (Val 4)) `shouldBe` 3
    -- it "" $ value (Add (Mult (Val (-2)) (Val 3)) (Val 4)) `shouldBe` (-2)

  describe "Prop" $ do
    let tbl::Subst
        tbl = [('A', True), ('B', False)]
        p1 = And (Var 'A') (Not (Var 'A'))
        p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
        p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
        p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
     in do
      it "" $ findK 'A' tbl `shouldBe` True
      it "" $ eval0 tbl (Const True) `shouldBe` True
      it "" $ eval0 tbl (Var 'A') `shouldBe` True
      it "" $ eval0 tbl (And (Var 'B') (Var 'A')) `shouldBe` False
      it "" $ eval0 tbl p1 `shouldBe` False
      it "" $ eval0 [('A', True), ('B', False)] p2 `shouldBe` True
      it "" $ eval0 [('A', True), ('B', True)] p2 `shouldBe` True
      it "" $ vars p1 `shouldBe` ['A']
      it "" $ vars p2 `shouldBe` ['A','B']
      it "" $ bools p1 `shouldBe` [False, False]
      it "" $ tautology p1 `shouldBe` False
      it "" $ tautology p2 `shouldBe` True
      it "" $ tautology p3 `shouldBe` False
      it "" $ tautology p4 `shouldBe` True

  describe "Tree" $ do
    let t1 = Node Leaf 1 Leaf
        xs = [1..8]
        -- ys = zip (reverse [1..4]) [5..9]
        -- ys

     in do
      it "" $ tree2lst t1 `shouldBe` [1]
      it "" $ tree2lst (conj t1 2) `shouldBe` [1,2]
      it "" $ tree2lst (lst2tree xs)  `shouldBe` xs
      it "" $ tree2lst (conj (lst2tree xs) 1)  `shouldBe` 1:xs
      it "" $ occurs 4 (lst2tree xs) `shouldBe` True
      it "" $ occurs 5 (lst2tree [1..4]) `shouldBe` False
      it "" $ treeHeight (lst2tree [1..4]) `shouldBe` 4
      it "" $ balanced (lst2tree [1..4]) `shouldBe` False
      it "" $ balanced (lst2tree [1..4]) `shouldBe` False
      it "" $ balanced (balance [1..7]) `shouldBe` True
      it "" $ balanced (balance [1..8]) `shouldBe` True

  describe "Exercise2" $ do
    let n = Succ (Succ Zero)
        a = int2nat 2
        b = int2nat 3
     in do
      it "" $ nat2int n `shouldBe` 2
      it "" $ nat2int (int2nat 3) `shouldBe` 3
      it "" $ nat2int (add a b) `shouldBe` 5
      it "" $ nat2int (mult a b) `shouldBe` 6
      it "" $ nat2int (mult a Zero) `shouldBe` 0

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
