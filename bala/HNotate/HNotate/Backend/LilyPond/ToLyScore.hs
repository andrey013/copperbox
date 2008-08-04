{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.LilyPond.ToLyScore
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Render an (general) Score to a LilyPond-tailored representation,
-- that is easier to emit LilyPond from.
--
--------------------------------------------------------------------------------

module HNotate.Backend.LilyPond.ToLyScore (
    lyscore
  )where

import HNotate.Backend.LilyPond.LyScoreDatatypes
import HNotate.Base.OnsetQueue
import HNotate.Score.Datatypes
import HNotate.Score.MeasureOnsets
import HNotate.Score.Utils

import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Sequence
import Prelude hiding (null)



instance OnsetEvent LyScMeasure LyScMeasure where
  onset m@(LyScMeasure i _ _) = (i, m)

lyscore :: ScScore -> LyScScore
lyscore (ScScore se) = LyScScore $ F.foldl fn mempty se
  where fn acc e = acc |> part e

part :: ScPart -> LyScPart
part p@(ScPart i _ _) = LyScPart i (polycat $ deriveQueue p)


-- Folding takes us too far into the OnsetQueue to 'evt' rather than '[evt]'.
-- So we have to use direct recursion
-- TODO - build bigger segments if the pattern of the voices stays the same
polycat :: OnsetQueue OnsetMeasure -> LyScLine
polycat = rec mempty . viewH
  where
    rec acc EmptyQ          = acc
    rec acc ((_,es) :>> q)  = rec (acc |*> trans es) (viewH q)

    trans :: [OnsetMeasure] -> Maybe LyScPolyPhrase
    trans = polyPhrase . map (LyScSegment . singleton . measure)


polyPhrase :: [LyScSegment] -> Maybe LyScPolyPhrase
polyPhrase []   = Nothing
polyPhrase [x]  = Just $ LyScSingletonPhrase x
polyPhrase xs   = Just $ LyScPolyPhrase xs


measure :: OnsetMeasure -> LyScMeasure
measure (OnsetMeasure i voice se) =
  LyScMeasure i voice (fmap glyph se)


glyph :: ScGlyph -> LyScGlyph
glyph (ScNote scp dur)          = LyScNote scp dur
glyph (ScRest dur)              = LyScRest dur
glyph (ScSpacer dur)            = LyScSpacer dur
glyph (ScChord xs dur)          = LyScChord xs dur
glyph (ScGraceNotes xs)         = LyScGraceNotes xs








