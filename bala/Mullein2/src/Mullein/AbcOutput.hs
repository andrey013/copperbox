{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print ABC.
--
--------------------------------------------------------------------------------


module Mullein.AbcOutput where

import Mullein.Core
import Mullein.Duration
import Mullein.Pitch hiding (pitch, octave)

import Text.PrettyPrint.Leijen hiding ( (<$>) )

import Data.Ratio


class AbcOutput e where
  type AbcDur e :: *
  abcNote  :: e -> AbcDur e -> Doc
  abcPitch :: e -> Doc


class AbcGlyph e where
  abcGlyph :: e -> Doc



instance AbcGlyph (Glyph Pitch AbcMultiplier) where
  abcGlyph = oGlyph 

instance AbcOutput Pitch where
  type AbcDur Pitch = AbcMultiplier
  abcNote p d = printNote p d
  abcPitch = pitch

instance AbcOutput ScNote where
  type AbcDur ScNote = AbcMultiplier
  abcNote (ScNote p _) d = printNote p d
  abcPitch (ScNote p _) = pitch p



{-
oBarOverlay :: AbcGlyph e => Bar e -> Doc
oBarOverlay (Bar xs) = hsep (map omBeam xs)
oBarOverlay _        = error "oBarOverlay TODO"
-}

oPhrase :: AbcGlyph e => Phrase e -> DPhrase
oPhrase = map oBarOverlay


oBarOverlay :: AbcGlyph e => Bar e -> DBar
oBarOverlay (Bar xs)       = [hsep $ map omBeam xs]
oBarOverlay (OverlayL xss) = map (hsep . map omBeam) xss




omBeam :: AbcGlyph e => Pulse e -> Doc
omBeam (Pulse e)    = abcGlyph e
omBeam (BeamedL es) = hcat $ map abcGlyph es



oGlyph :: (AbcOutput e, AbcDur e ~ AbcMultiplier) => Glyph e AbcMultiplier -> Doc
oGlyph (Note p dm)      = abcNote p dm
oGlyph (Rest dm)        = char 'z' <> multiplier dm
oGlyph (Spacer dm)      = char 'x' <> multiplier dm
oGlyph (Chord ps dm)    = brackets $ hcat $ map f ps where
                              f p = abcPitch p <> multiplier dm 
oGlyph (GraceNotes xs)  = braces $ hcat $ map f xs where
                              f (GraceNote p dm) = abcPitch p <> multiplier dm
oGlyph Tie              = char '~'

--------------------------------------------------------------------------------
-- rewriting


-- Change Duration to a multiplier

class ChangeDurationAbc t where
  changeDurationAbc :: Rational -> t Duration -> t AbcMultiplier

 
rewriteDuration :: ChangeDurationAbc t 
                => Rational 
                -> Phrase (t Duration) 
                -> Phrase (t AbcMultiplier)
rewriteDuration r bars = map (fmap (changeDurationAbc r)) bars

instance ChangeDurationAbc (Glyph pch) where
  changeDurationAbc r (Note p d)       = Note p $ abcMultiplier r d
  changeDurationAbc r (Rest d)         = Rest $ abcMultiplier r d
  changeDurationAbc r (Spacer d)       = Spacer $ abcMultiplier r d
  changeDurationAbc r (Chord ps d)     = Chord ps $ abcMultiplier r d
  changeDurationAbc r (GraceNotes xs)  = GraceNotes $ changeGraceD r xs
  changeDurationAbc _ Tie              = Tie

changeGraceD :: Rational 
             -> [GraceNote note Duration] 
             -> [GraceNote note AbcMultiplier]
changeGraceD r xs = map fn xs where
  fn (GraceNote p d) = GraceNote p (abcMultiplier r d)


-- Pitch spelling

class ChangePitchAbc t where
  changePitchAbc :: SpellingMap -> t Pitch drn -> t Pitch drn

 
rewritePitch :: ChangePitchAbc t 
             => SpellingMap 
             -> Phrase (t Pitch drn) 
             -> Phrase (t Pitch drn)
rewritePitch smap bars = map (fmap (changePitchAbc smap)) bars


instance ChangePitchAbc Glyph where
  changePitchAbc sm (Note p d)       = Note (spell sm p) d
  changePitchAbc _  (Rest d)         = Rest d
  changePitchAbc _  (Spacer d)       = Spacer d
  changePitchAbc sm (Chord ps d)     = Chord (map (spell sm) ps) d
  changePitchAbc sm (GraceNotes xs)  = GraceNotes (map (changePitchAbc sm) xs)
  changePitchAbc _  Tie              = Tie


instance ChangePitchAbc GraceNote where
  changePitchAbc sm (GraceNote p d) = GraceNote (spell sm p) d



--------------------------------------------------------------------------------

field :: Char -> Doc -> Doc
field ch d = char ch <> colon <> d



simpleOutput :: DPhrase -> Doc
simpleOutput = vsep . map ((<+> singleBar) . overlay)

singleBar :: Doc
singleBar = char '|'

overlay :: [Doc] -> Doc
overlay = vsep . punctuate (text " & ")    

printNote :: Pitch -> AbcMultiplier -> Doc 
printNote p m = pitch p <> multiplier m


data PitchChar = UPPER | LOWER
  deriving (Eq,Show)


-- Mullein - middle c is C5
-- Abc - middle c is 'C' upper case c  
pitch :: Pitch -> Doc
pitch (Pitch l a o) 
    | o > 5     = pitchLabel (PitchLabel l a) LOWER <> octave o 
    | otherwise = pitchLabel (PitchLabel l a) UPPER <> octave o 
  where
    octave :: Int -> Doc
    octave i  | i > 6       = text (replicate (i-6) '\'') 
              | i < 5       = text (replicate (5-i) ',')
              | otherwise   = empty


pitchLabel :: PitchLabel -> PitchChar -> Doc
pitchLabel (PitchLabel l a) pc 
    | pc == LOWER   = (maybe empty accidental a) <> (char . toLowerLChar) l
    | otherwise     = (maybe empty accidental a) <> (char . toUpperLChar) l
  where     
    accidental :: Accidental -> Doc
    accidental Nat           = char '='    
    accidental Sharp         = char '^' 
    accidental Flat          = char '_' 
    accidental DoubleSharp   = text "^^"
    accidental DoubleFlat    = text "__"


multiplier :: AbcMultiplier -> Doc
multiplier IdenM      = empty
multiplier (Mult n)   = integer n
multiplier (Div n)    = char '/' <> integer n
multiplier (Frac n d) = integer n <> char '/' <> integer d




