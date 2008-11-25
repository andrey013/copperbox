

-- ghci ...
-- :set -i../../Bala:../../ZMidi:../../HNotate

module BossaNova where

import Bala.Base
import Bala.MusicRep.Pulse
import Bala.MusicRep.DrumPitches

import ZMidi (writeMidi)

hi_hat :: ClavePattern
hi_hat  = clavel $ replicate 16 ClaveOn

cymbal :: ClavePattern
cymbal = readClave 'X' $    "X..X..X." ++ "..X..X.."

bass  :: ClavePattern
bass   = readClave 'X' $    "X..XX..X" ++ "X..XX..X"

bossa_nova :: Section
bossa_nova = section (2,4) $ overlay 
    [ drum hi_hat closed_hi_hat 
    , drum cymbal splash_cymbal
    , drum bass   bass_drum_1  ]  
  where
    drum pat inst = claveMotif (\d -> note inst d) sixteenth pat

    
genMidi :: IO ()
genMidi = writeMidi "./out/bossa_nova.mid" bossa_midi
  where
    bossa_midi = generateMidi (Just bossa_nova) []
         
    
main = do 
    genMidi 
    