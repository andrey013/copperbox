

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
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language



    
elements :: Read a => String -> [a]
elements = map read . words

  


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

token :: Parser a -> Parser a
token p = p <* many (oneOf " \t\n")

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

int               :: Parser Int
int               = fromIntegral <$> integer 


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

  