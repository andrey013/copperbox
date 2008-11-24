

-- ghci ...
-- :set -i../../HNotate:../../Bala:../../ZMidi

module SambaBaiao where

import Bala.Base
import Bala.MusicRep.Pulse
import Text.PrettyPrint.HughesPJ

import ZMidi (writeMidi)

sb_tap :: MotifF Clave
sb_tap  = readClave 'X' $    ".X.X.X.X" ++ ".X.X.XX."

sb_foot :: MotifF Clave
sb_foot = readClave 'X' $    "....X.X." ++ "....X..X"



samba_baiao_clave :: ClaveSection
samba_baiao_clave = section (2,4) $ overlay [sb_tap,sb_foot]

samba_baiao_d :: RhythmicSection
samba_baiao_d = claveUnitDuration sixteenth samba_baiao_clave


samba_baiao :: Section
samba_baiao = unitNote c4 samba_baiao_d


-- how to transform a particular overlay?


genMidi :: IO ()
genMidi = writeMidi "./out/samba_baiao.mid" samba_midi
  where
    samba_midi = generateMidi samba_baiao 
    
    
main = do 
    genMidi 