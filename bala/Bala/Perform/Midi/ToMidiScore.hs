--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Midi.ToMidiScore
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

module Bala.Perform.Midi.ToMidiScore where

import Bala.Format.Score
import Bala.Perform.Base.OnsetQueue
import Bala.Perform.Midi.MidiScoreDatatypes
import Bala.Perform.Score.MeasureOnsets
import Bala.Perform.Score.Utils

import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Sequence

midiscore :: ScScore pch dur -> MidiScScore pch dur
midiscore (ScScore se) = MidiScScore $ F.foldl fn mempty se 
  where fn acc e = acc |> track e
  
track :: ScPart pch dur -> MidiScTrack pch dur
track p@(ScPart i _ _) = MidiScTrack i (polycat $ deriveQueue p) 


-- | Just layout polyphonic measures one after another 
-- (tracking measure number is crucial though).
polycat :: OnsetQueue (OnsetMeasure pch dur) -> MidiScLine pch dur
polycat = rec mempty . viewH
  where
    rec acc EmptyQ          = acc
    rec acc ((_,es) :>> q)  = rec (acc >< trans es) (viewH q) 
    
    trans :: [OnsetMeasure pch dur] -> Seq (MidiScMeasure pch dur)
    trans = fromList . map measure



measure :: OnsetMeasure pch dur -> MidiScMeasure pch dur
measure (OnsetMeasure i voice se) = 
  MidiScMeasure i voice (fmap glyph (normalizeGroupedElements se))



glyph :: ScGlyph pch dur -> MidiScGlyph pch dur
glyph (ScNote scp dur)          = MidiScNote (pitch scp) dur
glyph (ScRest dur)              = MidiScSpacer dur
glyph (ScSpacer dur)            = MidiScSpacer dur
glyph (ScGroup ScChord xs)      = MidiScChord (catMaybes $ map justNote xs)
glyph (ScGroup ScGraceNotes xs) = MidiScGraceNotes (catMaybes $ map justNote xs)



justNote :: ScGlyph pch dur -> Maybe (MidiScGlyph pch dur)                     
justNote (ScNote scp dur)   = Just $ MidiScNote (pitch scp) dur                   
justNote _                  = Nothing   


pitch :: ScPitch pch -> pch
pitch (ScPitch pch) = pch
  
   