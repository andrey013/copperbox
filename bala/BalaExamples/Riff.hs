{-# LANGUAGE MultiParamTypeClasses #-}
-- :set -i..


module Main where

import Bala.Base 
import Bala.Perform.EventTree
import Bala.Perform.PerformBase
import Bala.Perform.PerformAbc
import Bala.Perform.PerformLilyPond
import Bala.Perform.PerformMidi

import Bala.Format.Output.OutputLilyPond hiding (chord) 







instance Perform Pitch Pitch Duration where
  eventvalues p = (Just p, Just eighth)
 

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
      env     = default_ly_env { initial_ly_context = expr }
      ly_expr = renderLy1 riff env
  in lilypond_template "Riff" ly_expr


riff_abc = 
  let abc_expr  = renderAbc1 riff default_abc_env
  in abc_template (abc_header_defaults {abc_title = "Riff"} ) abc_expr
    

main = do
    writeMidi "out/riff.midi" riff_midi
    writeLy "out/riff.ly" riff_ly
    execLilyPondOn "out/riff.ly"
    writeAbc "out/riff-abc.abc" riff_abc
    execAbcm2psOn "out/riff-abc.abc" "out/riff-abc.ps"
  where
    riff_midi = renderMidi1 riff default_midi_env 
