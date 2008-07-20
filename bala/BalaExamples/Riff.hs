
-- :set -i..


module Main where

import Bala.Base 
import Bala.Perform.EventTree
import Bala.Perform.PerformClass
import Bala.Perform.PerformAbc
import Bala.Perform.PerformLilyPond
import Bala.Perform.PerformMidi

import Bala.Format.Output.OutputLilyPond hiding (Pitch, chord, Duration) 







instance Perform Pitch where
  opitch = Just 
  oduration _ = Just eighth
 

bar_1 :: EventTree Pitch -> EventTree Pitch
bar_1 = 
  repeated 4 (chord [a3,e4]) #. event g3 #. event a3 #. event c4 #. event a3

bar_2 :: EventTree Pitch -> EventTree Pitch
bar_2 = 
  repeated 4 (chord [e3,b3]) #. event c4 #. event d4 #. event e4 #. event c4


riff :: EventTree Pitch  
riff  = root # bar_1 # bar_2 # bar_1 # chord [a3,e4]

riff_ly = 
  let expr    = elementBlk +++ key _c major +++ clef treble
      env     = withRelativePitch c4 st_zero
      ly_expr = renderLy1 expr riff env
  in lilypond_template "Riff" ly_expr


riff_abc = 
  let env       = abcEnv eighth (4//4)
      abc_expr  = renderAbc1 empty_body riff env
  in abc_template "Riff" abc_expr
    

main = do
    writeMidi "riff.midi" riff_midi
    writeLy "riff.ly" riff_ly
    execLilyPondOn "riff.ly"
    writeAbc "riff-abc.abc" riff_abc
    execAbcm2psOn "riff-abc.abc" "riff-abc.ps"
  where
    riff_midi = renderMidi1 riff default_midi_st 
