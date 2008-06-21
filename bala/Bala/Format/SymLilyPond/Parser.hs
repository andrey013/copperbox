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

import Bala.Format.Base.SymBase
import Bala.Format.SymLilyPond.Datatypes
import Bala.Format.SymLilyPond.SyntaxElements

import Bala.Base.BaseExtra

import Prelude hiding (break)
import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad

import Text.ParserCombinators.Parsec hiding (space)




-- | Pitch, plus attributes (knot tied).
pitchA :: (SymPitch repr, 
           SymAttrAccidental repr, 
           SymAttrMicroTone repr,
           SymAttrOctaveSpec repr) => 
          Parser (repr (Pitch ctx))
pitchA = pPitch >>= pAccidental >>= pMicroTone>>= pOctaveSpec 

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
    
   
pOctaveSpec :: (AttrOctaveSpec a, SymAttrOctaveSpec repr) =>
               repr (a ctx) -> Parser (repr (a ctx))
pOctaveSpec a = pRaised <|> pLowered
  where
    pRaised  = ((flip raised) a)  <$> counting1 (char '\'')
    pLowered = ((flip lowered) a) <$> counting1 (char ',')


pAccidental :: (SymAttrAccidental repr, AttrAccidental a) =>
               repr (a ctx) -> Parser (repr (a ctx))
pAccidental a = f <$> longestString accidentals
  where
    
    accidentals = [ "isis", "eses", "is", "es" ]
    f "isis" = a # doubleSharp 
    f "eses" = a # doubleFlat
    f "is"   = a # sharp
    f "es"   = a # flat


pMicroTone :: (SymAttrMicroTone repr, AttrMicroTone a) =>
               repr (a ctx) -> Parser (repr (a ctx))
pMicroTone a = fchoice string $ 
  [("ih", a # halfFlat), ("eh", a # halfFlat)] 

    
        
    
pKeyType :: (SymCmdKeyType repr) => Parser (repr (CmdKeyType CT_Element)) 
pKeyType = fchoice command $ 
  [ ("major",       major),   ("minor",       minor),
    ("ionian",      ionian),  ("locrian",     locrian),
    ("aeolian",     aeolian), ("mixolydian",  mixolydian),
    ("lydian",      lydian),  ("phrygian",    phrygian),
    ("dorian",      dorian)]
          

    
-- *** Manual beams (6.5.6)

pOpenBeam, pCloseBeam :: (SymBeam repr) => Parser (repr (Beam ctx)) 
pOpenBeam   = openBeam <$ lexChar '['
pCloseBeam  = closeBeam <$ lexChar ']'


-- ***  Articulations (6.6.1)
verticalPlacement :: (SymVerticalPlacement repr) => 
                     Parser (repr (VerticalPlacement ctx)) 
verticalPlacement = fchoice char $
  [('^',vabove), ('_',vbelow), ('-',vdefault)] 


    
--------------------------------------------------------------------------------
-- helpers 
--------------------------------------------------------------------------------


fchoice f xs = choice $ map (interp f) xs
  where
    interp f (a,b) = b <$ f a






--------------------------------------------------------------------------------
-- lexer combinators 
--------------------------------------------------------------------------------

-- | command - note using try is essential otherwise we might the forward
-- slash will be consumed 
command :: String -> CharParser st String
command ss = lexeme $ try $ string ('\\':ss)


    