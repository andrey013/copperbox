

-- ghci ...
-- :set -i../../Bala:../../HNotate:../../ZMidi

module GuitarDemo where

import Bala.Base
import Bala.MusicRep.GuitarNotation
import Bala.Base.OutputMidi

import ZMidi (writeMidi, GMInst(..) )

-- \fret-diagram-terse #"7-1-(;9-3;7-1-);8-2;x;x;" 
chordB7 = stChord (ffbs 7 1) (ff 9 3) (ffbe 7 1) (ff 8 2) x x

demo01 = fdiagram $ getST_Chord chordB7

animateChord :: [Pitch] -> Motif
animateChord ps = motif +- chord ps quarter ++- arpeggio ps where
  arpeggio = motifl . fmap (note `flip` eighth) 

section_b7 :: Section
section_b7 = section (4,4) $ phrase (animateChord $ pitchValue $ chordB7)

genMidi :: IO ()
genMidi = writeMidi "./out/chord_b7.mid" b7_midi
  where
    b7_midi = generateMidi Nothing [(Steel_acoustic_guitar, section_b7)]
    
main = do 
    genMidi
