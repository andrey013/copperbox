--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.LilyPond.Parser
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Parser for a subset of LilyPond files
--
--------------------------------------------------------------------------------

module Bala.Format.LilyPond.Parser where

import Bala.Base.BaseExtra
import Bala.Format.LilyPond.Datatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad

import Text.ParserCombinators.Parsec hiding (space)


rest :: Parser Rest
rest = Rest <$> (char 'r' *> duration)

duration :: Parser Int
duration = int


pitch = Pitch <$> pitchLetter <*> optparse octaveSpec

pitchLetter = oneOf "cdefgab"

octaveSpec :: Parser OctaveSpec
octaveSpec = raised <|> lowered
  where
    raised  = Raised  <$> counting1 (char '\'')
    lowered = Lowered <$> counting1 (char ',')
    
accidental :: Parser Accidental
accidental = choice [ss, ff, s, f] 
  where
    ss = fn DoubleSharp "isis"
    ff = fn DoubleFlat  "eses"
    s  = fn Sharp       "is"
    f  = fn Flat        "es"
    
    fn cnstr str = cnstr <$ lexeme (string str)
    
microTone :: Parser MicroTone
microTone = choice [hs, hf] 
  where
    hs = fn HalfSharp "ih"
    hf = fn HalfFlat  "eh"
    
    fn cnstr str = cnstr <$ lexeme (string str)  
    
stringMark :: Parser String
stringMark = char '^' *> stringLiteral

chord :: Parser [Pitch]
chord = angles (sepBy1 pitch whiteSpace)



       