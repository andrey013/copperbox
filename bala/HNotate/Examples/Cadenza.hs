

-- :set -i..:../HNotate
-- abc > M:none

module Main where

import HNotate


instance Event Pitch where
  eventvalues p = (Just $ renderPitch p, quarter)
  
notes = [c4, d4, e4, f4, c4, e4, d4, f4, e4, g4, f4, e4, d4, f4, e4, d4, c4] 

intro = [c4,e4,c4,e4]

cadenzaLy = systemL $ 
  [ ("intro", eventlist intro),
    ("cadenza1", eventlist notes) 
  ]
ly_template   = "templates/ly0-cadenza.ly"
ly_output     = "out/ly-cadenza.ly" 

cadenzaAbc = system1 "cadenza" (eventlist notes)
abc_template   = "templates/abc0-cadenza.abc"
abc_output     = "out/abc-cadenza.abc" 

main = do
    outputLilyPond cadenzaLy   ly_template   ly_output
    outputAbc      cadenzaAbc  abc_template  abc_output
    
    
debug = do
    dumpLyTemplates   ly_template  
    dumpAbcTemplates  abc_template  