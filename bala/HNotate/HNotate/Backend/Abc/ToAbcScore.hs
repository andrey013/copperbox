
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.Abc.ToAbcScore
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Render an (general) Score to an Abc-tailored representation,
-- that is easier to emit Abc from.
--
--------------------------------------------------------------------------------


module HNotate.Backend.Abc.ToAbcScore (
    abcscore
  ) where

import HNotate.Backend.Abc.AbcScoreDatatypes
import HNotate.Base.OnsetQueue
import HNotate.Score.Datatypes
import HNotate.Score.MeasureOnsets
import HNotate.Score.Utils

import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Sequence

abcscore :: ScScore -> AbcScTuneBook
abcscore (ScScore se) = AbcScTuneBook $ F.foldl fn mempty se
  where fn acc e = acc |> tune e

tune :: ScPart -> AbcScTune
tune p@(ScPart i _ _) = AbcScTune i (polycat $ deriveQueue p)


-- Folding takes us too far into the OnsetQueue evt rather than [evt]
-- So we have to use direct recursion
-- TODO - build bigger segments if the pattern of the voices stays the same
polycat :: OnsetQueue OnsetMeasure -> AbcScLine
polycat = rec mempty . viewH
  where
    rec acc EmptyQ          = acc
    rec acc ((_,es) :>> q)  = rec (acc |*> cnstr es) (viewH q)



    cnstr :: [OnsetMeasure] -> Maybe AbcScPolyPhrase
    cnstr = polyPhrase . map measure


polyPhrase :: [AbcScMeasure] -> Maybe AbcScPolyPhrase
polyPhrase []   = Nothing
polyPhrase [x]  = Just $ AbcScSingletonPhrase x
polyPhrase xs   = Just $ AbcScPolyPhrase xs



measure :: OnsetMeasure -> AbcScMeasure
measure (OnsetMeasure i voice se) =
  AbcScMeasure i voice (fmap glyph se)


glyph :: ScGlyph -> AbcScGlyph
glyph (ScNote scp dur)          = AbcScNote scp dur
glyph (ScRest dur)              = AbcScRest dur
glyph (ScSpacer dur)            = AbcScSpacer dur
glyph (ScChord xs dur)          = AbcScChord xs dur
glyph (ScGraceNotes xs)         = AbcScGraceNotes xs







