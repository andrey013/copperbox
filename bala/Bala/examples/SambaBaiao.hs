

-- ghci ...
-- :set -i../../Bala:../../ZMidi:../../HNotate

module SambaBaiao where

import Bala.Base
import Bala.MusicRep.Pulse
import Bala.Base.OutputMidi

import ZMidi (writeMidi, GMInst(..) )

sb_tap_clave :: ClavePattern
sb_tap_clave  = readClave 'X' $    ".X.X.X.X" ++ ".X.X.XX."

sb_foot_clave :: ClavePattern
sb_foot_clave = readClave 'X' $    "....X.X." ++ "....X..X"

samba_baiao :: Section
samba_baiao = section (2,4) $ phrase $ claveMotif sixteenth $ 
    [ (a4,    sb_tap_clave)
    , (c4,    sb_foot_clave)
    ]
  

    
genMidi :: IO ()
genMidi = writeMidi "./out/samba_baiao.mid" samba_midi
  where
    samba_midi = generateMidi Nothing [(Electric_piano_1,samba_baiao)] 
    
    
main = do 
    genMidi 