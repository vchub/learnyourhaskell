{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch13 where


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




















