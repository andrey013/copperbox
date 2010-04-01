{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.AbcOutput
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print ABC.
--
--------------------------------------------------------------------------------


module Neume.Core.AbcOutput 
  (

    AbcGlyph
  , AbcNote
  , ABC_Std_Rewrite_Config(..)
  , abcRewrite

  , renderPhrase

  -- * rewriting
  , rewritePitch
  , rewriteDuration
  , rewriteAnno

  ) where

import Neume.Core.AbcBasic
import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.Pitch hiding ( octave )
import Neume.Core.SyntaxScore
import Neume.Core.SyntaxStaff
import Neume.Core.Utils
import Neume.Core.Utils.OneList ( OneList, toListF )

import Text.PrettyPrint.Leijen hiding ( sep )     -- package: wl-print

import qualified Data.Foldable as F
import Data.Sequence ( Seq )

-- No annos for Abc...

type AbcGlyph = Glyph () Pitch AbcMultiplier
type AbcNote  = Note  () Pitch AbcMultiplier


data ABC_Std_Rewrite_Config = ABC_Std_Rewrite_Config 
    { spelling_map    :: SpellingMap
    , unit_duration   :: DurationMeasure 
    }

abcRewrite :: ABC_Std_Rewrite_Config 
           -> StaffPhrase (Glyph anno Pitch Duration)
           -> StaffPhrase AbcGlyph
abcRewrite cfg = rewriteDuration (unit_duration cfg) 
               . rewritePitch    (spelling_map cfg) 
               . rewriteAnno 


--------------------------------------------------------------------------------

renderPhrase :: StaffPhrase AbcGlyph -> PhraseImage
renderPhrase = mapInto oStaffBar . extractBars

oStaffBar :: StaffBar AbcGlyph -> BarImage
oStaffBar = oCExprSeq (<+>)

oCExprSeq :: (Doc -> Doc -> Doc) -> Seq (CExpr AbcGlyph) -> Doc
oCExprSeq sep se = sepList sep $ mapInto (oCExpr sep) se


oCExprList :: (Doc -> Doc -> Doc) -> [CExpr AbcGlyph] -> Doc
oCExprList sep xs = sepList sep $ map (oCExpr sep) xs

-- This one needs 'context' - beamed notes are no whitespace
-- between them.
--
oCExpr :: (Doc -> Doc -> Doc) -> CExpr AbcGlyph -> Doc
oCExpr _  (Atom e)        = oGlyph e
oCExpr op (N_Plet pm xs)  = pletContext (pletStats pm (length xs))
                         <+> oCExprList op xs
oCExpr _  (Beamed notes)  = oCExprList (<>) notes

pletStats :: PletMult -> Int -> (Int,Int,Int)
pletStats (n,d) len = (fromIntegral d, fromIntegral n,len)

oGlyph :: AbcGlyph -> Doc
oGlyph (GlyNote n t)        = oNote n <> optDoc t tie
oGlyph (Rest dm)            = rest dm
oGlyph (Spacer dm)          = spacer dm
oGlyph (Chord ps dm t)      = (chordForm $ oChordPitches dm ps) 
                              <> optDoc t tie
oGlyph (Graces xs)          = graceForm $ toListF oNote xs


oNote :: AbcNote -> Doc
oNote (Note _ p dm) = note p dm


-- print each note with the same multiplier
--
-- > [C8E8G8]
--
oChordPitches :: AbcMultiplier -> OneList (ChordPitch anno Pitch) -> [Doc]
oChordPitches dm = map (\(ChordPitch _ p) -> note p dm) . F.toList


--------------------------------------------------------------------------------
-- Rewrite duration

rewriteDuration :: Rational 
                -> FreeRewrite (Glyph anno pch Duration) 
                               (Glyph anno pch AbcMultiplier)
rewriteDuration r = fmap (fmap3c (abcMultiplier r))


--------------------------------------------------------------------------------
-- Rewrite Pitch

-- Pitch spelling

rewritePitch :: SpellingMap 
             -> FreeRewrite (Glyph anno Pitch dur) (Glyph anno Pitch dur)
rewritePitch sm = fmap (fmap3b (spell sm))


--------------------------------------------------------------------------------
-- Rewrite Anno

-- Drop annotations

rewriteAnno :: FreeRewrite (Glyph anno pch dur) (Glyph ()   pch dur)
rewriteAnno = fmap (fmap3a (const ()))


--------------------------------------------------------------------------------


