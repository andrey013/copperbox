--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.Midi.ToMidiScore
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Render an (general) Score to a Midi-tailored representation,
-- that is easier to emit Midi from.
-- Note, processing related to onsets, delta times etc. is left to the final
-- output stages in the Midi backend.
--
--------------------------------------------------------------------------------

module HNotate.Backend.Midi.ToMidiScore (
    midiscore
  ) where

import HNotate.Backend.Midi.MidiScoreDatatypes
import HNotate.Base.Datatypes
import HNotate.Base.OnsetQueue
import HNotate.Score.Datatypes
import HNotate.Score.MeasureOnsets
import HNotate.Score.Utils

import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Sequence

midiscore :: ScScore -> MidiScScore
midiscore (ScScore se) = MidiScScore $ F.foldl fn mempty se
  where fn acc e = acc |> track e

track :: ScPart -> MidiScTrack
track p@(ScPart i _ _) = MidiScTrack i (polycat $ deriveQueue p)


-- | Just layout polyphonic measures one after another
-- (tracking measure number is crucial though).
polycat :: OnsetQueue OnsetMeasure -> MidiScLine
polycat = rec mempty . viewH
  where
    rec acc EmptyQ          = acc
    rec acc ((_,es) :>> q)  = rec (acc >< trans es) (viewH q)

    trans :: [OnsetMeasure] -> Seq MidiScMeasure
    trans = fromList . map measure



measure :: OnsetMeasure -> MidiScMeasure
measure (OnsetMeasure i voice se) =
  MidiScMeasure i voice (fmap glyph se)



glyph :: ScGlyph -> MidiScGlyph
glyph (ScNote scp dur)          = MidiScNote scp dur
glyph (ScRest dur)              = MidiScSpacer dur
glyph (ScSpacer dur)            = MidiScSpacer dur
glyph (ScChord se dur)          = MidiScChord se dur
glyph (ScGraceNotes se)         = MidiScGraceNotes se







