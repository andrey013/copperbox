

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.BaseExtra
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Useful functions
-- |
--------------------------------------------------------------------------------


module Bala.Base.BaseExtra where

import Control.Applicative hiding (many, optional)
import Control.Monad (ap)

import Data.Char (ord)
import Data.List (mapAccumL)

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language



-- clashes with QuickCheck and needs a better name anyway    
-- elements :: Read a => String -> [a]
-- elements = map read . words


  
{-
-- zack, like zac but the interval measure is counts from 1 
-- (i.e 4-4 has an interval of 1, 4-5 interval 2, 4-6 interval 3)
zack :: (Num a) => a -> [a] -> [a]
zack i xs = snd $ mapAccumL fn i (0:xs)
  where fn acc n  = (acc + n - 1, acc + n)
-}

-- ,scanl shiftyPlus, replaces zack  
shiftyPlus :: (Num a) => a -> a -> a
shiftyPlus a b = a - 1 + b  
  

-- zam - zippy map
zam :: (a -> a -> b) -> [a] -> [b]
zam f (x:y:ys) = f x y : zam f (y:ys)
zam f _        = []


mod12 i = i `mod` 12
mod7  i = i `mod` 7  


  
  
--------------------------------------------------------------------------------
-- Parsec helpers
--------------------------------------------------------------------------------


instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap
  
  
readsParsec :: (Parser a) -> String -> [(a,String)]
readsParsec p s = case parse pfn "" s of
                    Left _ -> []
                    Right a -> [a] 
  where pfn = (,) <$> p <*> getInput

lexeme :: Parser a -> Parser a
lexeme p = p <* many (oneOf " \t\n")

optOneOf :: [Char] -> Parser (Maybe Char)    
optOneOf cs = optparse $ oneOf cs

optparse :: Parser a -> Parser (Maybe a)
optparse p = option Nothing (Just <$> p)

positiveInt :: Parser Int
positiveInt = read <$> many1 digit

signedInt :: Parser Int
signedInt   = (\ a b -> read (a:b)) <$> sign <*> many1 digit
  where sign        = oneOf "+-"


baseLex             = P.makeTokenParser emptyDef


whiteSpace        = P.whiteSpace baseLex 
parens            = P.parens baseLex
integer           = P.integer baseLex
double            = P.float baseLex


float             :: Parser Float
float             = fromRational . toRational <$> double

int               :: Parser Int
int               = fromIntegral <$> integer 

digiti            :: Parser Int
digiti            = (flip (-) 48) . ord  <$> digit


--------------------------------------------------------------------------------
-- Show helpers
--------------------------------------------------------------------------------

showOpt :: (Show a) => Maybe a -> ShowS
showOpt Nothing = id
showOpt (Just a) = shows a

showSpace :: ShowS
showSpace = showChar ' '

showDot :: ShowS
showDot = showChar '.'

showNl = showChar '\n'

withParens :: ShowS -> ShowS
withParens f = showChar '(' . f . showChar ')'


showTogether :: (Show a) => [a] -> ShowS
showTogether = foldr cat id
  where cat :: (Show a) => a -> ShowS -> ShowS
        cat a acc = (shows a) . acc

caten :: [ShowS] -> ShowS
caten = foldr (.) id


catenSep :: [ShowS] -> ShowS
catenSep [] = id
catenSep (x:xs) = foldl fn x xs
  where fn e acc = e . showSpace . acc
