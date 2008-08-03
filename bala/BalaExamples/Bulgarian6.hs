{-# LANGUAGE MultiParamTypeClasses #-}

-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- :set -i..

module Main where


import Bala.Base
import Bala.Format.Output.OutputLilyPond
import Bala.Perform.PerformOriginal
import Bala.Perform.OriginalMidi
import Bala.Perform.OriginalLilyPond


import Text.PrettyPrint.Leijen hiding (dot)

data NrEvent = Note Pitch Duration
             | Rest Duration
  deriving (Eq,Show)



instance Perform NrEvent Pitch Duration where
  eventvalues (Note p d) = (Just p, Just d)
  eventvalues (Rest d)   = (Nothing, Just d)
  
  
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
      [ n a4 16, n b4 16, n c5is 16, n c5is 16, 
        n c5is 16, n a4 16, n c5is 16, n c5is 16,
        
        n c5is 16, n a4 16, n b4 16, n c5is 16,
        n b4 16, n a4 16, n a4 16, r 16,
        
        n e5 16, n d5 16, n c5is 16, n b4 16,
        n c5is 16, n a4 16, n b4 16, n c5is 16,
        
        n a4 16, n b4 16, n b4 16, n a4 16,
        n a4 8, r 8       
      ]
         

bars1_4 :: EventTree NrEvent
bars1_4 = foldl (flip event) root events_bars1_4
 

bulgarian6 = (Perf [bars1_4])   
  

    
       
-------

-- LilyPond handling is very unpolished
  

bulgarian_template musicexpr = 
    toplevelStart
      +++ version "2.10.3" 
      +++ header (headerStart +++ title "Bulgarian (6)")
      +++ book
            (block (score 
                      (block (relative (_c ! raised 1) musicexpr))))
  

bulgarian6_ly = 
  let expr    = elementStart +++ key _a major +++ clef treble
      env     = default_ly_env { initial_ly_context = expr }
      ly_expr = renderLy1 bars1_4 env
  in bulgarian_template ly_expr
   

    
demo_ly = printLy bulgarian6_ly

main =  do
    writeMidi "out/bulgarian6.midi" bulgarian6_midi
    writeLy lyfile bulgarian6_ly
    execLilyPondOn lyfile
  where
    lyfile = "out/bulgarian6.ly" 
    
    bulgarian6_midi = renderMidi bulgarian6 default_midi_env  
  
  
  
  
   