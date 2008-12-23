{-# OPTIONS -Wall #-}

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

import Bala.Base.Duration
import Bala.Base.Pitch
import Bala.Base.Structural hiding (note, chord)

import ZMidi hiding (Event)

import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.Sequence
import Data.Word

generateMidi :: Maybe Section -> [(GMInst,Section)] -> MidiFile
generateMidi (Just perc)  []  = execConstruction (midiDrums perc) 384 120 

generateMidi (Just perc)  ss  = execConstruction actions 384 120 
  where 
    actions = do { midiDrums perc; newTrack; midiSections ss }

generateMidi Nothing      ss  = execConstruction (midiSections ss) 384 120 
  

midiSections :: [(GMInst,Section)] -> OutputMidi ()
midiSections = sequence_ . intersperse newTrack . fmap (uncurry midiSection)

midiSection :: GMInst -> Section -> OutputMidi ()
midiSection inst sn = do 
    midiLines inst $ linearTransform sn

 
midiDrums :: Section -> OutputMidi ()
midiDrums sn = do 
    drumTrack
    F.mapM_ outputPerc (linearTransform sn) 
    nextChannel
  where
    outputPerc se = F.mapM_ outputEvent se >> clockToZero
    
    
-- each line becomes a channel
midiLines :: GMInst -> Seq (Seq Event) -> OutputMidi ()
midiLines inst sse = F.mapM_ outputChannel sse where
  outputChannel se = do 
      programChange (fromIntegral $ fromEnum inst) 
      F.mapM_ outputEvent se
      nextChannel

        
outputEvent :: Event -> OutputMidi ()
outputEvent (NoteE p d)         = note (midiPitch p) (ticks d)
outputEvent (RestE d)           = spacer (ticks d)
outputEvent (ChordE se d)       = chord (F.foldr fn [] se) (ticks d) where
                                      fn p xs = (midiPitch p) : xs
outputEvent (SpacerE d)         = spacer (ticks d)    
outputEvent (AGraceE _ p d)     = note (midiPitch p) (ticks d) -- to do /se/
outputEvent (UGraceE p d _)     = note (midiPitch p) (ticks d) -- to do /se/
outputEvent (MarkE _)           = return ()

durationGraces :: (Seq (Pitch, Duration)) -> Duration
durationGraces = F.foldr (\e n -> snd e + n) 0



midiPitch :: Pitch -> Word8
midiPitch = fromIntegral . (+12) . semitones

ticks :: Duration -> Word32
ticks drn | drn == no_duration = 0
          | otherwise        = fn $ ratioElements $ convRational drn
  where
    fn (n,1) = n * midi_wn
    fn (1,d) = midi_wn `div` d
    fn (n,d) = (n * midi_wn) `div` d  

midi_qn         :: Word32
midi_qn         = 384

midi_wn         :: Word32
midi_wn         = midi_qn * 4

            