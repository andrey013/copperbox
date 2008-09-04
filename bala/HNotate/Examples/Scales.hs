-- ghci ...
-- :set -i../../HNotate:../../ZMidi

module Main where


import HNotate

instance Event Pitch where
  eventvalues p = (Just $ renderPitch p, Just $ quarter)
  
  
cmajor_notes = [c4, d4, e4, f4, g4, a4, b4, c5, d5, e5, f5, g5, a5, b5, c6]

gmajor_notes = [g3, a3, b3, c4, d4, e4, fis4, g4, a4, b4, c5, d5, e5, fis5, g5]

major_scales = systemL $ 
    [("cmajor", notelist cmajor_notes),
     ("gmajor", notelist gmajor_notes)]

template_file = "templates/ly0-major-scales.ly"
output_file   = "out/ly-major-scales.ly"


main = outputLilyPond major_scales template_file output_file