-- ghci ...
-- :set -i../../HNotate

module Examples.Scales where


import HNotate

  
  
cmajor_notes = [c4, d4, e4, f4, g4, a4, b4, c5, d5, e5, f5, g5, a5, b5, c6]

gmajor_notes = [g3, a3, b3, c4, d4, e4, fis4, g4, a4, b4, c5, d5, e5, fis5, g5]

major_scales = systemL $ 
    [("cmajor", simpleEventlist cmajor_notes quarter),
     ("gmajor", simpleEventlist gmajor_notes quarter)]

ly_template   = "templates/ly0-major-scales.ly"
ly_output     = "out/ly-major-scales.ly"

abc_template  = "templates/abc0-major-scales.abc"
abc_output    = "out/abc-major-scales.abc"

outputScales :: IO ()
outputScales = do 
    outputLilyPond 3 major_scales     ly_template     ly_output
    outputAbc      3 major_scales     abc_template    abc_output

  
    
    