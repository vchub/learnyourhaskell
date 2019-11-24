{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch13 where

import           Control.Applicative
import           Data.Char
import           System.IO

newtype Parser a = P (String->[(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P
  (\inp -> case inp of
    []       -> []
    (x : xs) -> [(x, xs)]
  )

instance Functor Parser where
  -- fmap::(a->b)-> Parser a -> Parser b
  fmap g p = P( \inp -> case parse p inp  of
      []         -> []
      [(v, out)] -> [(g v, out)]
      (_:_:_)    -> error "shouldn't be the case")

instance Applicative Parser where
  -- pure::a->Parser a
  pure v = P(\inp -> [(v, inp)])

  -- (<*>):: Parser (a->b) -> Parser a -> Parser b
  pg <*> px = P( \inp -> case parse pg inp of
     []        -> []
     [(g,out)] -> parse (fmap g px) out
     -- [(g,out)] -> case parse px out of
     --     []->[]
     --     [(v,outa)]-> [(g v, outa)]
     (_:_:_)   -> error "Applicative shouldn't be the case"
         )

three :: Parser (Char, Char)
-- parse three chars, skipping the 2nd
three = pure g <*> item <*> item <*> item where g x _ z = (x, z)
-- three =
--   pure (\(x : _ : z) -> (x, z)) <*> P (\inp -> [(take 3 inp, drop 3 inp)])


instance Monad Parser where
  -- (>>=)::Parser a -> (a->Parser b)-> Parser b
  p >>= f = P(\inp -> case parse p inp of
    []        -> []
    [(v,out)] -> parse (f v) out
    (_:_:_)   -> error "Monad shouldn't be the case")


three1 :: Parser (Char, Char)
-- parse three chars, skipping the 2nd
three1 = do
  x <- item
  _ <- item
  z <- item
  return (x, z)


instance Alternative Parser where
  -- empty::f a
  empty = P(\_ -> [])

  -- (<|>)::f a->f a->f a
  p <|> q = P(\inp -> case parse p inp of
    []    -> parse q inp
    (x:_) -> [x]           )

sat :: (Char -> Bool) -> Parser Char
sat g = do
  v <- item
  if g v then return v else empty

letter :: Parser Char
letter = sat isAlpha

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x : xs) = do
  _ <- char x
  _ <- string xs
  return (x : xs)

some1 :: Parser a -> Parser [a]
some1 p = pure (:) <*> p <*> many1 p

many1 :: Parser a -> Parser [a]
many1 p = some1 p <|> pure []

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

ident :: Parser String
ident = do
  x  <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  x <- some digit
  return (read x)

space :: Parser ()
space = do
  _ <- many (sat isSpace)
  return ()

int :: Parser Int
-- int = char '-' <*> nat <*> pure negate <|> nat
-- int = fmap negate (char '-' <*> nat) <|> nat
int = negnat <|> nat
 where
  negnat = do
    _ <- char '-'
    n <- nat
    return (-n)

token :: Parser a -> Parser a
token p = do
  _ <- space
  x <- p
  _ <- space
  return x

identfier :: Parser String
identfier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol s = token (string s)

nats :: Parser [Int]
nats = do
  _  <- symbol "["
  x  <- natural
  xs <- some
    (do
      _ <- symbol ","
      n <- natural
      return n
    )
  _ <- symbol "]"
  return (x : xs)

-- takeWhileP :: (Char -> Bool) -> Parser String
-- takeWhileP g = do
--   c <- item
--   if g c
--     then do
--       cs <- takeWhileP g
--       return (c : cs)
--     else return [c]

comment :: Parser ()
comment = do
  _ <- symbol "--"
  _ <- many (sat (/= '\n'))
  _ <- item
  return ()





-- ====================
-- Arigthmetic expression Parser

expr :: Parser Int
expr = sumexp <|> term
 where
  sumexp =
    (do
      x  <- term
      op <- symbol "+" <|> symbol "-"
      y  <- expr
      case op of
        "+" -> return (x + y)
        "-" -> return (x - y)
        _   -> empty
    )

term :: Parser Int
term = prodexp <|> factor
 where
  prodexp =
    (do
      x  <- factor
      op <- symbol "*" <|> symbol "/"
      y  <- expr
      case op of
        "*" -> return (x * y)
        "/" -> return (x `div` y)
        _   -> empty
    )

factor :: Parser Int
factor = parentheses <|> integer
 where
  parentheses =
    (do
      _ <- symbol "("
      e <- expr
      _ <- symbol ")"
      return e
    )

eval :: String -> Int
eval xs = case parse expr xs of
  [(n, [] )] -> n
  [(_, out)] -> error ("Unused input " ++ out)
  []         -> error ("Invalid input")
  (_ : _)    -> error ("eval: Shouldn't be the case")

-- ====================
-- Calculator

buttons :: String
buttons = standard ++ extra
 where
  standard = "qcd=123+456-789*0()/"
  extra    = "QCD \ESC\BS\DEL\n"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ (show y) ++ ";" ++ (show x) ++ "H")

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

calc :: String -> IO ()
calc xs = do
  c <- getCh
  if elem c buttons
    then process c xs
    else do
      beep
      calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = eval1 xs
             | elem c "cC"        = clear
             | otherwise          = press c xs


beep :: IO ()
beep = putStr "\BEL"

quit :: IO ()
quit = putStrLn "Buy"

delete :: String -> IO ()
delete [] = calc []
delete xs = do
  cls
  writeat (1, 1) (init xs)
  calc (init xs)

eval1 :: String -> IO ()
eval1 xs = case parse expr xs of
  [(n, [])] -> do
    cls
    calc (show n)
  _ -> do
    beep
    calc xs

press :: Char -> String -> IO ()
press c xs = do
  cls
  writeat (1, 1) (xs ++ [c])
  calc (xs ++ [c])

clear :: IO ()
clear = do
  cls
  calc []

run :: IO ()
run = do
  cls
  calc ""








