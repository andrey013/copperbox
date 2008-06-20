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
import Bala.Format.SymLilyPond.SyntaxElements

import Bala.Base.BaseExtra

import Prelude hiding (break)
import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad

import Text.ParserCombinators.Parsec hiding (space)



pPitch :: (SymPitch repr) => Parser (repr (Pitch ctx))
pPitch = choice $ map fn xs
  where
    fn (ch,cnstr) = cnstr <$ char ch   
    xs = [('c',      _c),
          ('d',      _d),
          ('e',      _e),
          ('f',      _f),
          ('g',      _g),
          ('a',      _a),
          ('b',      _b)]
    
   



pKeySignature :: (SymCmdKeyType repr) => Parser (repr (CmdKeyType CT_Element)) 
pKeySignature = choice $ map (uncurry nullaryCommand) xs
  where  
    xs = [("major",       major),
          ("minor",       minor),
          ("ionian",      ionian),
          ("locrian",     locrian),
          ("aeolian",     aeolian),
          ("mixolydian",  mixolydian),
          ("lydian",      lydian),
          ("phrygian",    phrygian),
          ("dorian",      dorian)]
          

    

pOpenBeam, pCloseBeam :: (SymBeam repr) => Parser (repr (Beam ctx)) 
pOpenBeam   = openBeam  <$ lchar '['
pCloseBeam  = closeBeam <$ lchar ']'

--------------------------------------------------------------------------------
-- helpers 
--------------------------------------------------------------------------------

nullaryCommand :: String -> (repr (a ctx)) -> Parser (repr (a ctx)) 
nullaryCommand name cstr = cstr <$ command name


--------------------------------------------------------------------------------
-- lexer combinators 
--------------------------------------------------------------------------------

-- | command - note using try is essential otherwise we might the forward
-- slash will be consumed 
command :: String -> CharParser st String
command ss = lexeme $ try $ string ('\\':ss)

lchar ::Char -> CharParser st Char
lchar ch = lexeme (char ch)
    