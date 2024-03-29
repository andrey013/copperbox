
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- ghci ...
-- :set -i../../HNotate

module Bulgarian6 where


import HNotate

   

-- a major


bars1_4 :: EventList
bars1_4 = 
    root # note a4 du16   # note b4 du16 # note cis5 du16 # note cis5 du16 
         # note cis5 du16 # note a4 du16 # note cis5 du16 # note cis5 du16
        
         # note cis5 du16 # note a4 du16 # note b4 du16   # note cis5 du16
         # note b4 du16   # note a4 du16 # note a4 du16   # rest du16
        
         # note e5 du16   # note d5 du16 # note cis5 du16 # note b4 du16
         # note cis5 du16 # note a4 du16 # note b4 du16   # note cis5 du16
        
         # note a4 du16   # note b4 du16 # note b4 du16   # note a4 du16
         # note a4 du8    # rest du8       

        
 
bulgarian6 :: System
bulgarian6 = system1 "bulgarian6" bars1_4   
  
ly_template, ly_output :: FilePath
ly_template   = "templates/ly0-bulgarian6.ly"
ly_output     = "out/ly-bulgarian6.ly" 

abc_template, abc_output :: FilePath
abc_template  = "templates/abc0-bulgarian6.abc"
abc_output    = "out/abc-bulgarian6.abc"    


main :: IO ()
main = do 
    outputLilyPond DebugOn bulgarian6   ly_template   ly_output
    outputAbc      DebugOn bulgarian6   abc_template  abc_output
    
 


    