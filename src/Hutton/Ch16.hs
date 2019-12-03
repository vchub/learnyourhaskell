{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch16 where

import           Control.Applicative
import qualified Hutton.Ch13         as P

data Expr = Val Int | Add Expr Expr | Sub Expr Expr deriving (Eq, Show)

eval0 :: Expr -> Int
eval0 (Val n  ) = n
eval0 (Add l r) = (eval0 l) + (eval0 r)
eval0 (Sub l r) = (eval0 l) - (eval0 r)

eval1 :: String -> Int
eval1 xs = case P.parse expr xs of
  [(e, [] )] -> eval0 e
  [(_, out)] -> error ("Unused input " ++ out)
  []         -> error "Invalid input"
  (_ : _)    -> error "eval: Shouldn't be the case"

expr :: P.Parser Expr
expr = sumexp <|> term
 where
  sumexp = do
    x  <- term
    op <- P.symbol "+" <|> P.symbol "-"
    y  <- expr
    case op of
      "+" -> return (Add x y)
      "-" -> return (Sub x y)
      _   -> empty

term :: P.Parser Expr
term = val

val :: P.Parser Expr
val = do
  n <- P.integer
  return (Val n)

type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD | SUB | THROW | CATCH deriving (Show, Eq)

comp :: Expr -> Code
comp e = go e []
 where
  go (Val n  ) st = PUSH n : st
  go (Add l r) st = go l (go r (ADD : st))
  go (Sub l r) st = go l (go r (SUB : st))

exec :: Code -> Stack -> Stack
exec []               st           = st
exec (PUSH n : codes) st           = exec codes (n : st)
exec (ADD    : codes) (x : y : st) = exec codes (y + x : st)
exec (SUB    : codes) (x : y : st) = exec codes (y - x : st)
exec _                _            = error "Unknow Op"

eval2 :: String -> Int
eval2 xs = head (exec (compile xs) [])

compile :: String -> Code
compile xs = case P.parse expr xs of
  [(e, [] )] -> comp e
  [(_, out)] -> error ("Unused input " ++ out)
  []         -> error "Invalid input"
  (_ : _)    -> error "eval: Shouldn't be the case"


-- ====================
-- Ch18

type Cont = Stack ->Stack




























