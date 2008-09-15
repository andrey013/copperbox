
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- ghci ...
-- :set -i../../HNotate:../../ZMidi

module Examples.Bulgarian6 where


import HNotate

import Text.PrettyPrint.Leijen hiding (dot)
     

-- a major

du16 = sixteenth
du8  = eighth

bars1_4 :: EventList
bars1_4 = 
    root # note a4 du16   # note b4 du16 # note cis5 du16 # note cis5 du16 
         # note cis5 du16 # note a4 du16 # note cis5 du16 # note cis5 du16
        
         # note cis5 du16 # note a4 du16 # note b4 du16 # note cis5 du16
         # note b4 du16   # note a4 du16 # note a4 du16 # rest du16
        
         # note e5 du16   # note d5 du16 # note cis5 du16 # note b4 du16
         # note cis5 du16 # note a4 du16 # note b4 du16   # note cis5 du16
        
         # note a4 du16 # note b4 du16 # note b4 du16 # note a4 du16
         # note a4 du8  # rest du8       

        
 
bulgarian6 :: System
bulgarian6 = system1 "bulgarian6" bars1_4   
  

ly_template   = "templates/ly0-bulgarian6.ly"
ly_output     = "out/ly-bulgarian6.ly" 

abc_template  = "templates/abc0-bulgarian6.abc"
abc_output    = "out/abc-bulgarian6.abc"    


main = do 
    outputLilyPond bulgarian6   ly_template   ly_output
    outputAbc      bulgarian6   abc_template  abc_output
    
debug = do 
    dumpAbcTemplates  abc_template
    dumpLyTemplates   ly_template
  
  
  
   