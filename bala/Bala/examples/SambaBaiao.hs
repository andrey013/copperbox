

-- ghci ...
-- :set -i../../Bala:../../ZMidi:../../HNotate

module SambaBaiao where

import Bala.Base
import Bala.MusicRep.Pulse

import ZMidi (writeMidi, GMInst(..) )

sb_tap :: ClavePattern
sb_tap  = readClave 'X' $    ".X.X.X.X" ++ ".X.X.XX."

sb_foot :: ClavePattern
sb_foot = readClave 'X' $    "....X.X." ++ "....X..X"

samba_baiao :: Section
samba_baiao = section (2,4) $ overlay 
    [ tap  sb_tap
    , foot sb_foot  ]  
  where
    foot pat = claveMotif (\d -> note c4 d) sixteenth pat
    tap pat  = claveMotif (\d -> chord [a4,d5,g5] d) sixteenth pat
    
    
genMidi :: IO ()
genMidi = writeMidi "./out/samba_baiao.mid" samba_midi
  where
    samba_midi = generateMidi Nothing [(Electric_piano_1,samba_baiao)] 
    
    
main = do 
    genMidi 