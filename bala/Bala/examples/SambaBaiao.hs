

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
-- topline isn't very generic

-- Note: topline can't change type because it doesn't touch overlays

topline :: (a -> a) -> SectionF a -> SectionF a
topline f (Section tm se) = Section tm (fmap (topline' f) se) where
  topline' f (Single mo)      = Single (fmap f mo)
  topline' f (Overlay mo smo) = Overlay (fmap f mo) smo
  
changeNote :: Event -> Event
changeNote (Note p d) | p == c4   = chord [a4,d5,g5] d
changeNote evt                    = evt

samba_baiao_final :: Section
samba_baiao_final = topline changeNote samba_baiao


genMidi :: IO ()
genMidi = writeMidi "./out/samba_baiao.mid" samba_midi
  where
    samba_midi = generateMidi samba_baiao_final 
    
    
main = do 
    genMidi 