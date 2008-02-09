

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Triad
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Triad representation
-- |
--------------------------------------------------------------------------------


module Bala.Base.Chord where

import Bala.Base.PitchRep
import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 


newtype Chord = Chord {unChord :: [Int]}

{-
instance Read Triad where 
  readsPrec i s = readP_to_S readTriad s

readTriad :: ReadP Triad  
readTriad = undefined
-}

data RomanCase = RUpper | RLower 
  deriving (Eq,Show)

readNumeral = lowerNumeral <|> upperNumeral

lowerNumeral :: Parser (Int, RomanCase)
lowerNumeral = romanNumeral Lower ('i', 'v', 'x')

upperNumeral :: Parser (Int, RomanCase)
upperNumeral = romanNumeral Upper ('I', 'V', 'X')


romanNumeral :: a -> (Char,Char,Char) -> Parser (Int,a)
romanNumeral a (i',v',x') = tryDigit >>= kont 0 >>= valid
  where
    valid 0 = fail "parse error romanNumeral"
    valid i = return (i,a)

    oI = maybe False ((==)  i')
    oV = maybe False ((==)  v')
    oX = maybe False ((==)  x')
    tryDigit :: Parser (Maybe Char)
    tryDigit = optOneOf [i',v',x']
    
    kont i Nothing      = return i  
    kont 0 od | oI od   = tryDigit >>= kont 1
    kont 0 od | oV od   = tryDigit >>= kont 5
    kont 0 od | oX od   = tryDigit >>= kont 10
    kont 1 od | oV od   = return 4
    kont 1 od | oX od   = return 9
    kont i od | oI od   = kontI i 
    
    
    kontI i | i >= 5  && i < 8    = tryDigit >>= kont (i+1)
            | i >= 10 && i < 14   = tryDigit >>= kont (i+1)
            | otherwise           = return i
            


    
    
readNumeral' = choice [two,one]
  where
    one = 1 <$ count 1 (char 'i')
    two = 2 <$ count 2 (char 'i')

data Numeral = N Int 
  deriving Show
    
readNumeralParsec :: Parser Numeral
readNumeralParsec = (N . length) <$> many1 (char 'i')

instance Read Numeral where 
  readsPrec i s = readsParsec readNumeralParsec s
  
  