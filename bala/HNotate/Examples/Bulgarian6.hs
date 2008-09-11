
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- ghci ...
-- :set -i../../HNotate:../../ZMidi

module Main where


import HNotate

import Text.PrettyPrint.Leijen hiding (dot)


data NrEvent = Note Pitch Duration
             | Rest Duration
  deriving (Eq,Show)



instance Event NrEvent where
  eventvalues (Note p d) = (Just $ renderPitch p, renderDuration d)
  eventvalues (Rest d)   = (Nothing, renderDuration d)
  
  
durn 16  = sixteenth
durn 8   = eighth
durn 4   = quarter
durn 2   = half
durn 1   = whole
      
n :: Pitch -> Int -> NrEvent
n p i = Note p (durn i)

r :: Int -> NrEvent    
r = Rest . durn

-- a major

events_bars1_4 :: [NrEvent]
events_bars1_4 = 
      [ n a4 16, n b4 16, n cis5 16, n cis5 16, 
        n cis5 16, n a4 16, n cis5 16, n cis5 16,
        
        n cis5 16, n a4 16, n b4 16, n cis5 16,
        n b4 16, n a4 16, n a4 16, r 16,
        
        n e5 16, n d5 16, n cis5 16, n b4 16,
        n cis5 16, n a4 16, n b4 16, n cis5 16,
        
        n a4 16, n b4 16, n b4 16, n a4 16,
        n a4 8, r 8       
      ]
         

bars1_4 :: EventList NrEvent
bars1_4 = eventlist events_bars1_4
 
bulgarian6 :: System NrEvent
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
  
  
  
   