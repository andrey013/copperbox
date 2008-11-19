

-- ghci ...
-- :set -i../../HNotate:../../Bala:../../ZMidi

module MeterDemo2 where

import Bala.Base.Duration
import Bala.Base.Pitch
import Bala.Base.OutputMidi2
import Bala.Base.Structural2

import Bala.Base.OutputHNotate

import HNotate hiding (note, rest)
import HNotate.DocLilyPond

import ZMidi (writeMidi)

attack d = note c4 d

a4    = attack du4
a4'   = attack (dotn 1 du4)
r4    = rest du4
a8    = attack du8
r8    = rest du8
a8'   = attack (dotn 1 du8)
a16   = attack du16
r16   = rest du16



sb_tap :: Motif 
sb_tap  = motif +- r16 +- a8 +- a16 +- r16 +- a8  +- a16 
                +- r16 +- a8 +- a16 +- r16 +- a16 +- a8

sb_foot = motif +- r4  +- a8 +- a4' +- a8' +- a16

samba_baiao = section (2,4) $ starts' [sb_tap, sb_foot]

genMidi :: IO ()
genMidi = writeMidi "./out/samba_baiao2.mid" samba_midi
  where
    samba_midi = generateMidi samba_baiao 


-- Not yet
genLy :: IO ()
genLy = outputLilyPondDocu 5 samba_sys samba_doc "./out/samba_baiao.ly"
  where
    samba_sys = system1 "samba_baiao"  samba_eventlist
    samba_eventlist = generateEventList samba_baiao    
    samba_doc = lilypond 
                  [ version
                  
                  , header                  $
                    title "Samba Baiao"     $
                    noExpr
                         
                  , definition "sambaBaiao" $
                    relative middle_c       $ 
                    time 2 4                $
                    key c_nat major         $
                    outputRelative "samba_baiao"
                  
                  , book $ score $ invocation "sambaBaiao"
                  ]  
    


    
main = do 
    genMidi 
    genLy
                 