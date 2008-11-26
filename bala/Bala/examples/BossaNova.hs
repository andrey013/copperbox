

-- ghci ...
-- :set -i../../Bala:../../ZMidi:../../HNotate

module BossaNova where

import Bala.Base
import Bala.Base.DrumPitches
import Bala.Base.OutputHNotate

import Bala.MusicRep.Pulse

import ZMidi (writeMidi)

import HNotate hiding (note, rest)
import HNotate.DocLilyPond




hi_hat_clave :: ClavePattern
hi_hat_clave  = clavel $ replicate 16 ClaveOn

cymbal_clave :: ClavePattern
cymbal_clave = readClave 'X' $    "X..X..X." ++ "..X..X.."

bass_clave  :: ClavePattern
bass_clave   = readClave 'X' $    "X..XX..X" ++ "X..XX..X"

{-
bossa_nova :: Section
bossa_nova = section (2,4) $ overlay 
    [ drum Hihat        hi_hat_clave
    , drum Ridecymbal   cymbal_clave
    , drum Bassdrum     bass_clave  ]  
  where
    drum inst pat = claveMotif (\d -> note (drumPitch inst) d) sixteenth pat
-}

--- DRUM CHORDS....

bossa_nova :: Section
bossa_nova = section (2,4) $ phrase $ claveMotif' sixteenth $ 
  [ (drumPitch Hihat,         hi_hat_clave)
  , (drumPitch Ridecymbala,   cymbal_clave)
  , (drumPitch Bassdrum,      bass_clave)
  ]
  

    
genMidi :: IO ()
genMidi = writeMidi "./out/bossa_nova.mid" bossa_midi
  where
    bossa_midi = generateMidi (Just bossa_nova) []
         
    
main = do 
    genMidi 
    genLy


genLy :: IO ()
genLy = outputLilyPondDocu 5 bossa_sys bossa_doc "./out/bossa_nova.ly"
  where
    bossa_sys = system1 "bossa_nova"  bossa_eventlist
    bossa_eventlist = genGenerateEventList drumFoldStep bossa_nova    
    bossa_doc = lilypond 
                  [ version
                  
                  , header                        $
                    title "Bossa Nova"            $
                    noExpr
                         
                  , definition "bossaNova"        $ 
                    time 2 4                      $
                    outputAbsolute "bossa_nova"
                  
                  , book $ score $ invocation "bossaNova"
                  ]  
    

    