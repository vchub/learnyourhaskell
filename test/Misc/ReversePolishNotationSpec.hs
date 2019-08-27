{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module ReversePolishNotationSpec where
-- import           Text.Read
import           Test.Hspec
-- import           Test.Hspec.QuickCheck

-- http://learnyouahaskell.com/functionally-solving-problems

calculate :: String -> Double
calculate = head . (\ws -> cal' ws []) . words

cal' :: [String] -> [Double] -> [Double]
cal' []       stack = stack
cal' (s : ss) stack = case reads s :: [(Double, String)] of
  [(x, _)] -> cal' ss (x : stack)
  _        -> cal' ss (handle s stack)
   where
    handle :: String -> [Double] -> [Double]
    handle op (x : y : rest) = ((toOp op) x y) : rest
    handle _  _              = undefined


toD :: String -> Double
toD = \x -> read x :: Double

toOp :: Floating a => String -> a -> a -> a
toOp "+"  = (+)
toOp "-"  = (-)
toOp "*"  = (*)
toOp "**" = (**)
toOp _    = undefined
-- toOp _ _ _   = error "Unknown operator"




spec :: Spec
spec = describe "ReversePolishNotation" $ do
  describe "read" $ do
    it "read 1" $ (read "1" :: Integer) `shouldBe` 1
    -- it "read +" $ (read "+" :: String) `shouldBe` "+"
    it "words 1 2 +" $ (words "1 2 +") `shouldBe` ["1", "2", "+"]

  describe "calculator" $ do
    it "calculate 1" $ calculate "1" `shouldBe` 1.0
    it "calculate 1 2 +" $ calculate "1 2 +" `shouldBe` 3.0
    it "calculate 3 1 2 + -" $ calculate "3 1 2 + -" `shouldBe` 0
    it "calculate 10 4 3 + 2 * -" $ calculate "10 4 3 + 2 * -" `shouldBe` 4
    it "calculate 10 4 3 + 2 * - 2 **"
      $          calculate "10 4 3 + 2 * - 2 **"
      `shouldBe` 16

-- splitAt 1 [1,2,3]
-- splitAt 2 [1,2,3]

-- read "1" :: Double
-- read "1" :: Double
-- reads "1 +" :: [(Double,String )]
-- reads "0.4" :: [(Double,String )]
-- reads ".4 +" :: [(Double,String )]
-- reads "1" :: [(Double,String )]
-- reads "+ 1" :: [(Double,String )]
-- reads "+1" :: [(Double,String )]
-- reads "1" :: [Double]



-- [1,"foo"]
-- (1,"foo")
-- show 3.4
-- words "3 4 +"
-- readMaybe "0.4" :: Maybe Double
-- readMaybe "0.4" :: Maybe String
-- readMaybe "+" :: Maybe Char
-- readMaybe "sss" :: Maybe Char
-- readMaybe "sss" :: Maybe String


