{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.AbcOutput
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


module Neume.AbcOutput where

import Neume.AbcDoc
import Neume.Doc
import Neume.Duration
import Neume.FunctorN
import Neume.OneList
import Neume.Pitch hiding ( octave )
import Neume.SyntaxDoc
import Neume.SyntaxStaff


import Text.PrettyPrint.Leijen hiding ( sep )     -- package: wl-print

import qualified Data.Foldable as F

-- No annos for Abc...

type AbcGlyph = Glyph () Pitch AbcMultiplier
type AbcNote  = Note  () Pitch AbcMultiplier

--------------------------------------------------------------------------------


oStaffPhrase :: StaffPhrase AbcGlyph -> AbcPhrase
oStaffPhrase                = AbcPhrase . map oStaffBar . getStaffPhrase


oStaffBar :: StaffBar AbcGlyph -> AbcBar
oStaffBar                   = AbcBar . oBarUnit . getStaffBar

oBarUnit :: OneList (CExpr AbcGlyph) -> Doc
oBarUnit os                 = hsep $ toListF (oCExpr hsep) os


-- This one needs 'context' - beamed notes are no whitespace
-- between them.
--
oCExpr :: ([Doc] -> Doc) -> CExpr AbcGlyph -> Doc
oCExpr sep (Atomic os)      = sep $ toListF oGlyph os 
oCExpr _   (N_Plet _ _)     = error $ "oCExpr - N_Plet to do"
oCExpr _   (Beamed e)       = oCExpr hcat e


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
                -> StaffPhrase (Glyph anno pch Duration) 
                -> StaffPhrase (Glyph anno pch AbcMultiplier)
rewriteDuration r = fmap (fmap3c (abcMultiplier r))


--------------------------------------------------------------------------------
-- Rewrite Pitch

-- Pitch spelling

rewritePitch :: SpellingMap
             -> StaffPhrase (Glyph anno Pitch dur) 
             -> StaffPhrase (Glyph anno Pitch dur)
rewritePitch sm = fmap (fmap3b (spell sm))


--------------------------------------------------------------------------------
-- Rewrite Anno

-- Drop annotations

rewriteAnno :: StaffPhrase (Glyph anno pch dur) 
            -> StaffPhrase (Glyph ()   pch dur)
rewriteAnno = fmap (fmap3a (const ()))


--------------------------------------------------------------------------------
{-

-- | Output ABC, four bars printed on each line. 
simpleOutput :: DPhrase -> Doc
simpleOutput = four . map ((<+> singleBar) . overlay)


four :: [Doc] -> Doc
four (a:b:c:d:xs) = vsep (map (<> lineCont) [a,b,c]) <$> d <$> four xs
four xs           = hsep xs

-}