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


module Mullein.AbcOutput 
  (
  -- * Glyph class
    AbcGlyph(..)

  -- * Render    
  , renderPhrase

  -- * Rewriting
  , ChangeDurationAbc(..)
  , rewriteDuration
  
  , ChangePitchAbc(..)
  , rewritePitch

  -- * Post-process and pretty print
  , simpleOutput 

  ) where

import Mullein.AbcDoc
import Mullein.Core
import Mullein.Duration
import Mullein.Pitch hiding ( octave )

import Text.PrettyPrint.Leijen

import Data.Ratio


--------------------------------------------------------------------------------
-- Classes

-- | To be renderable as ABC, glyphs must implement this class.
class AbcGlyph e where
  abcGlyph :: e -> Doc



instance AbcGlyph (Glyph Pitch AbcMultiplier) where
  abcGlyph (Note p dm)      = note p dm
  abcGlyph (Rest dm)        = rest dm
  abcGlyph (Spacer dm)      = spacer dm
  abcGlyph (Chord ps dm)    = chordForm $ map (note `flip` dm) ps 
  abcGlyph (GraceNotes xs)  = graceForm $ map abcGrace xs
  abcGlyph Tie              = tie

abcGrace :: GraceNote Pitch AbcMultiplier -> Doc
abcGrace (GraceNote p dm) = note p dm


--------------------------------------------------------------------------------


-- | Render a phrase. This function returns a 'DPhrase' which is 
-- a list of list of Doc. To generate output, it must be 
-- post-processed. One such post-processor is 'simpleOutput'...
renderPhrase :: AbcGlyph e => Phrase e -> DPhrase
renderPhrase = map oBarOverlay

oBarOverlay :: AbcGlyph e => Bar e -> DBar
oBarOverlay (Bar xs)       = [hsep $ map omBeam xs]
oBarOverlay (OverlayL xss) = map (hsep . map omBeam) xss

omBeam :: AbcGlyph e => Pulse e -> Doc
omBeam (Pulse e)    = abcGlyph e
omBeam (BeamedL es) = hcat $ map abcGlyph es



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
  changeDurationAbc r (GraceNotes xs)  = GraceNotes $ 
                                           map (changeDurationAbc r) xs
  changeDurationAbc _ Tie              = Tie

instance ChangeDurationAbc (GraceNote pch) where
  changeDurationAbc r (GraceNote p d) = GraceNote p (abcMultiplier r d)


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

-- | Output ABC, four bars printed on each line. 
simpleOutput :: DPhrase -> Doc
simpleOutput = four . map ((<+> singleBar) . overlay)


four :: [Doc] -> Doc
four (a:b:c:d:xs) = vsep (map (<> lineCont) [a,b,c]) <$> d <$> four xs
four xs           = hsep xs

