
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
    ScBlock(..),
    ScMeasure(..),
    ScGlyph(..),
    CommonGlyph(..),
    
    -- * Aliases
    DSystem,
    DStrata,
    DBlock,
    DMeasure,
    DGlyph,
    Glyph

  ) where

import CommonUtils (sepSeq)
import Duration
import Pitch

import Data.Foldable (toList)
import Data.Sequence (Seq, ViewL(..), viewl)
import Text.PrettyPrint.Leijen


type DSystem    = ScSystem Glyph Duration
type DStrata    = ScStrata Glyph Duration
type DBlock     = ScBlock Glyph Duration
type DMeasure   = ScMeasure Glyph Duration
type DGlyph     = ScGlyph Glyph Duration

type Glyph      = CommonGlyph Duration



newtype ScSystem e d = ScSystem { getSystem :: Seq (ScStrata e d) }


-- A 'horizontal line' of music - of course it maybe printed on 
-- more than one line and might contain polyphony, but it will be played 
-- by a single instrument.
data ScStrata e d = ScStrata {
    _strata_number    :: Int,
    _strata_chunks    :: Seq (ScBlock e d)
  }



-- Follow the Abc style when voice overlays are grouped measure-wise.
-- The Int holds the measure number
data ScBlock e d = ScSingleBlock Int (ScMeasure e d)
                 | ScPolyBlock Int (Seq (ScMeasure e d))


newtype ScMeasure e d = ScMeasure { getMeasure :: Seq (ScGlyph e d) }


-- have glyph open so we can have say, lyrics...
data ScGlyph e d = ScGlyph e d


data CommonGlyph dur = CmnNote Pitch
                     | CmnRest
                     | CmnSpacer  -- non-printed rest
                     | CmnChord (Seq Pitch)
                     | CmnGraceNotes (Seq (Pitch,dur))



instance (Pretty e, Pretty d) => Pretty (ScSystem e d) where
  pretty (ScSystem se) = (sepSeq (<$>) se) <> line

instance (Pretty e, Pretty d) => Pretty (ScStrata e d) where
  pretty (ScStrata i se) = prefix i <$> text ":line " <> int i
                                    <$> sepSeq (<$>) se
    where
      prefix i  = let l = snd $ intPlex i in text $ replicate (l+6) '-'


instance (Pretty e, Pretty d) => Pretty (ScBlock e d) where
  pretty (ScSingleBlock i e) = measureNumber i
                                         <$> indent 4 (pretty e)
  pretty (ScPolyBlock i se)  = 
      measureNumber i <$> indent 4 (encloseSep (text "<<") 
                                               (text ">>") 
                                               (text " // ")
                                               (map pretty $ toList se))

measureNumber :: Int -> Doc
measureNumber i = text "|:" <>  int i


instance (Pretty e, Pretty d) => Pretty (ScMeasure e d) where
  pretty (ScMeasure se) = sepSeq (</>) se

instance (Pretty e, Pretty d) => Pretty (ScGlyph e d) where 
  pretty (ScGlyph e d) = pretty e <> char '/' <> pretty d

instance (Pretty d) => Pretty (CommonGlyph d) where
  pretty (CmnNote pch)       = pretty pch 
  pretty (CmnRest)           = char 'r'
  pretty (CmnSpacer)         = char 'S'
  pretty (CmnChord ps)       = brackets $ sepSeq (<>) ps
  pretty (CmnGraceNotes ps)  = braces $ sepSeq (<>) ps


intPlex i = let s = show i in (s,length s)

tagint i = let (s,l) = intPlex i in
  if l < 5 then text (replicate (5-l) '0' ++ s) else text s

