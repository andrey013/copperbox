
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ScoreRepresentation
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for Score format
--
-- Polyphony is indicated at the start of a measure by pointers to a
-- dictionary of lines (a sequence of measures). These lines may themselves
-- have measures that point to further polyphonic lines.
--
--------------------------------------------------------------------------------

module ScoreRepresentation (
    -- * Datatypes
    ScSystem(..),
    ScStrata(..),
    ScChunk,
    ScMeasure(..),
    ScGlyph(..),
    CommonGlyph(..)
  ) where

import CommonUtils (sepSeq)
import Duration
import Pitch

import Data.Sequence (Seq, ViewL(..), viewl)
import Text.PrettyPrint.Leijen



data ScSystem gly dur = ScSystem (Seq (ScStrata gly dur))


-- A 'horizontal line' of music - of course it maybe printed on 
-- more than one line and might contain polyphony, but it will be played 
-- by a single instrument.
data ScStrata gly dur = ScStrata {
    _strata_number    :: Int,
    _strata_chunks    :: ScChunk gly dur
  }


type ScChunk gly dur = Seq (ScMeasure gly dur)



-- simultaneous measures (i.e. polyphony) have the same measure number
-- but different polyhonic 'indexes'.
data ScMeasure gly dur = ScMeasure {
    _measure_number :: Int,
    _poly_index     :: Int,
    _measure_glyphs :: Seq (ScGlyph gly dur)
  }



-- have glyph open so we can have say, lyrics...
data ScGlyph gly dur = ScGlyph gly dur




data CommonGlyph dur = CmnNote Pitch
                     | CmnRest
                     | CmnSpacer  -- non-printed rest
                     | CmnChord (Seq Pitch)
                     | CmnGraceNotes (Seq (Pitch,dur))



instance (Pretty gly, Pretty dur) => Pretty (ScSystem gly dur) where
  pretty (ScSystem se) = (sepSeq (<$>) se) <> line

instance (Pretty gly, Pretty dur) => Pretty (ScStrata gly dur) where
  pretty (ScStrata i se) = prefix i <$> text ":line " <> int i
                                       <$> sepSeq (<$>) se
    where
      prefix i  = let l = snd $ intPlex i in text $ replicate (l+6) '-'



instance (Pretty gly, Pretty dur) => Pretty (ScMeasure gly dur) where
  pretty (ScMeasure i v se) = text "|:" <>  int i <+> voice v
                                        <$> indent 4 (sepSeq (</>) se)
    where voice n = text "voice:" <> int n  

instance (Pretty gly, Pretty dur) => Pretty (ScGlyph gly dur) where 
  pretty (ScGlyph gly dur) = pretty gly <> char '/' <> pretty dur

instance (Pretty dur) => Pretty (CommonGlyph dur) where
  pretty (CmnNote pch)       = pretty pch 
  pretty (CmnRest)           = char 'r'
  pretty (CmnSpacer)         = char 'S'
  pretty (CmnChord ps)       = brackets $ sepSeq (<>) ps
  pretty (CmnGraceNotes ps)  = braces $ sepSeq (<>) ps


intPlex i = let s = show i in (s,length s)

tagint i = let (s,l) = intPlex i in
  if l < 5 then text (replicate (5-l) '0' ++ s) else text s

