

-- ghci ...
-- :set -i../../Bala:../../ZMidi:../../HNotate

module ChaCha where

import Bala.Base
import Bala.Base.DrumOutput 
import Bala.Base.OutputHNotate
import Bala.Base.OutputMidi

import Bala.MusicRep.TimbalesStyle
import Bala.MusicRep.Pulse

import ZMidi (writeMidi)

import HNotate hiding (note, rest)
import HNotate.DocLilyPond




hi_sidestick_clave    :: ClavePattern
hi_sidestick_clave    = readClave 'X' $    "X.XXX.XX"

cowbell_clave         :: ClavePattern
cowbell_clave         = readClave 'X' $    "X.X.X.X."

lo_timbale_clave      :: ClavePattern
lo_timbale_clave      = readClave 'X' $    "...X..X."

lo_sidestick_clave    :: ClavePattern
lo_sidestick_clave    = readClave 'X' $    ".X...X.."



cha_cha :: Section
cha_cha = section (2,4) $ phrase $ claveMotif sixteenth $ 
  [ (drumPitch Hisidestick,   hi_sidestick_clave)
  , (drumPitch Cowbell,       cowbell_clave)
  , (drumPitch Lotimbale,     lo_timbale_clave)
  , (drumPitch Losidestick,   lo_sidestick_clave)
  ]
  

    
genMidi :: IO ()
genMidi = writeMidi "./out/cha_cha.mid" cha_cha_midi
  where
    cha_cha_midi = generateMidi (Just cha_cha) []
         
    
main = do 
    genMidi 
    genLy


genLy :: IO ()
genLy = outputLilyPondDocu 5 chacha_sys chacha_doc "./out/cha_cha.ly"
  where
    chacha_sys = system1 "cha_cha"  chacha_eventlist
    chacha_eventlist = genGenerateEventList drumFoldStep cha_cha   
    chacha_doc = lilypond 
                  [   version
                  
                  ,   header
                    . title "Cha Cha"                   
                         
                  ,   definition "chaCha"
                    . drummode 
                    . time 2 4
                    . outputAbsolute "cha_cha"
                  
                  ,   book 
                    . score
                    . new "DrumStaff" 
                    . doubleAngles
                    . set "DrumStaff.drumStyleTable = #timbales-style"
                    . new "DrumVoice"
                    . expression                    
                    . lycommand "voiceOne"
                    . invocation "chaCha"
                  ]  
    

    