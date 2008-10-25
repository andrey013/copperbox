

-- :set -i..:../HNotate
-- abc > M:none

module Examples.Cadenza where

import HNotate


  
notes = [c4, d4, e4, f4, c4, e4, d4, f4, e4, g4, f4, e4, d4, f4, e4, d4, c4] 

intro = [c4,e4,c4,e4]

cadenzaLy = systemL $ 
  [ ("intro", notelist intro quarter),
    ("cadenza1", notelist notes quarter) 
  ]
ly_template   = "templates/ly0-cadenza.ly"
ly_output     = "out/ly-cadenza.ly" 

cadenzaAbc = system1 "cadenza" (notelist notes quarter)
abc_template   = "templates/abc0-cadenza.abc"
abc_output     = "out/abc-cadenza.abc" 

main :: IO ()
main = do
    outputLilyPond 5 cadenzaLy   ly_template   ly_output
    outputAbc      5 cadenzaAbc  abc_template  abc_output
    
