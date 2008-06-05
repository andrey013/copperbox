--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymLilyPond.Parser
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

module Bala.Format.SymLilyPond.Parser where

import Bala.Format.SymLilyPond.Datatypes
import Bala.Base.BaseExtra

import Prelude hiding (break)
import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad

import Text.ParserCombinators.Parsec hiding (space)



pitchName_ :: (SymPitchName repr) => Parser (repr PitchName)
pitchName_ = choice $ map fn xs
  where
    fn (ch,cnstr) = cnstr <$ char ch   
    xs = [('c',      c_),
          ('d',      d_),
          ('e',      e_),
          ('f',      f_),
          ('g',      g_),
          ('a',      a_),
          ('b',      b_)]
    
    
nullaryCommand :: (SymCmdZero repr) => 
                    String -> (repr CmdZero) -> Parser (repr CmdZero)
nullaryCommand name cstr = cstr <$ command name

major_, minor_    :: (SymCmdZero repr) => Parser (repr CmdZero)  
major_            = nullaryCommand "major" major
minor_            = nullaryCommand "minor" minor 


mode :: (SymCmdZero repr) => Parser (repr CmdZero) 
mode = choice $ map (uncurry nullaryCommand) xs
  where  
    xs = [("ionian",      ionian),
          ("locrian",     locrian),
          ("aeolian",     aeolian),
          ("mixolydian",  mixolydian),
          ("lydian",      lydian),
          ("phrygian",    phrygian),
          ("dorian",      dorian)]
     

openBeam_, closeBeam_ :: (SymBeam repr) => Parser (repr Beam) 
openBeam_   = openBeam  <$ lchar '['
closeBeam_  = closeBeam <$ lchar ']'

--------------------------------------------------------------------------------
-- lexer combinators 
--------------------------------------------------------------------------------

-- | command - note using try is essential otherwise we might the forward
-- slash will be consumed 
command :: String -> CharParser st String
command ss = lexeme $ try $ string ('\\':ss)

lchar ::Char -> CharParser st Char
lchar ch = lexeme (char ch)
    