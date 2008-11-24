
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.OutputMidi2
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Output Midi for Structural
--
--------------------------------------------------------------------------------



module Bala.Base.OutputMidi where

import Bala.Base.BaseExtra
import Bala.Base.Duration
import Bala.Base.Pitch
import Bala.Base.Structural hiding (note, chord)

import ZMidi hiding (Event)

import qualified Data.Foldable as F
import Data.Sequence
import Data.Word

generateMidi :: Section -> MidiFile
generateMidi sn = 
  execConstruction (midiLines $ linearTransform sn) 384 120 


-- each line becomes a channel
midiLines :: Seq (Seq Event) -> OutputMidi ()
midiLines sse = F.mapM_ outputChannel sse where
  outputChannel se =  F.mapM_ outputEvent se >> nextChannel

        
outputEvent :: Event -> OutputMidi ()
outputEvent (Note p d)          = note (midiPitch p) (ticks d)
outputEvent (Rest d)            = spacer (ticks d)
outputEvent (Chord se d)        = chord (F.foldr fn [] se) (ticks d) where
                                      fn p xs = (midiPitch p) : xs
outputEvent (Spacer d)          = spacer (ticks d)    
outputEvent (AGrace se p d)     = note (midiPitch p) (ticks d) -- to do
outputEvent (UGrace p d se)     = note (midiPitch p) (ticks d) -- to do
outputEvent (Mark _)            = return ()

durationGraces :: (Seq (Pitch, Duration)) -> Duration
durationGraces = F.foldr (\e n -> snd e + n) 0



midiPitch :: Pitch -> Word8
midiPitch = fromIntegral . (+12) . semitones

ticks :: Duration -> Word32
ticks d | d == no_duration = 0
        | otherwise        = fn $ ratioElements $ convRational d
  where
    fn (n,1) = n * midi_wn
    fn (1,d) = midi_wn `div` d
    fn (n,d) = (n * midi_wn) `div` d  

midi_qn         :: Word32
midi_qn         = 384

midi_wn         :: Word32
midi_wn         = midi_qn * 4

            