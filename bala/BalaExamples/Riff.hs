


-- ghci ...
-- :set -i..:../HNotate:../ZMidi


module Main where

import Bala.Base 
import HNotate
import HNotate.Print.OutputLilyPond hiding (chord, grace)



instance Event Pitch where
  eventvalues p = (Just $ renderPitch p, Just $ renderDuration eighth)
 

bar_1 :: EventTree Pitch -> EventTree Pitch
bar_1 = 
  repeated 4 (chord [a3,e4]) #. event g3 #. event a3 #. event c4 #. event a3

bar_2 :: EventTree Pitch -> EventTree Pitch
bar_2 = 
  repeated 4 (chord [e3,b3]) #. event c4 #. event d4 #. event e4 #. event c4


riff :: System Pitch  
riff  = system1 $ root # bar_1 # bar_2 # bar_1 # chord [a3,e4]

-- Currently, LilyPond output has as too large octave step on the second bar
 
riff_ly = systemToLy (default_ly_system "Riff" pre) riff
  where pre = elementStart +++ key _a major +++ clef treble
  

{-
riff_abc = 
  let abc_expr  = renderAbc1 riff default_abc_env
  in abc_template (abc_header_defaults {abc_title = "Riff"} ) abc_expr
-}    

sc1 = pretty $ toScore riff default_score_env

main = do
    writeMidi "out/riff.midi" riff_midi
    writeLy "out/riff.ly" riff_ly
    runLilyPondOn "out/riff.ly"
--    writeAbc "out/riff-abc.abc" riff_abc
--    execAbcm2psOn "out/riff-abc.abc" "out/riff-abc.ps"
  where
    riff_midi = systemToMidi default_midi_system riff