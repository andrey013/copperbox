{-# LANGUAGE MultiParamTypeClasses #-}

module Trees where

import Bala.Base hiding (toPitch)
import qualified Bala.Base as B
import Bala.Format.Output.AbcInternals (unAbc)
import Bala.Format.Output.LilyPondInternals (unLy)
import Bala.Perform.Perform
import Bala.Perform.Base.Datatypes (middleC, quarternote) 

import Text.PrettyPrint.Leijen





data NrEvent = Note Pitch Duration
             | Rest Duration
  deriving (Eq,Show)

du1,du2,du4,du8,du16 :: Pitch -> NrEvent
du1 p    = Note p whole
du2 p    = Note p half
du4 p    = Note p quarter
du8 p    = Note p eighth
du16 p   = Note p sixteenth



instance Perform NrEvent where
  eventvalues (Note p d) = (Just $ renderPitch p, Just $ renderDuration d)
  eventvalues (Rest d)   = (Nothing, Just $ renderDuration d)

runPerf :: Performance NrEvent -> IO ()
runPerf = putDoc . pretty . performanceToMidi

runLy :: Performance NrEvent -> IO ()
runLy = putDoc . vsep . map (pretty . unLy) . performanceToLy

runAbc :: Performance NrEvent -> IO ()
runAbc = putDoc . vsep . map (pretty . unAbc) . performanceToAbc


-- | example 1 - simple list of successive notes.  
example1 :: Performance NrEvent
example1 = perf1 $ 
    root # event (c4 # du4) # event (c4 # du4) # event (d4 # du4) 
         # event (e4 # du4)

print1 = runPerf example1
                

run1 :: IO ()
run1 = do
    writeMidi "out/example1.midi" example1_midi
  where
    example1_midi = performanceToMidi example1
    
    

   
-- | example 2 - simple parallel - a chord             
example2 :: Performance NrEvent
example2 = perf1 $ root # chord (map du1 [c4,e4,g4])

run2 :: IO ()
run2 = do
    writeMidi "out/example2.midi" example2_midi
  where
    example2_midi = performanceToMidi example2

print2 = runPerf example2

    
-- | example 3 - successors and grace.
example3 :: Performance NrEvent
example3 = perf1 $
  root # event (c4 # du4) # grace (map du16 [a4, b4]) 
       # event (c4 # du4) # event (d4 # du4) # event (e4 # du4)

run3 :: IO ()
run3 = do
    writeMidi "out/example3.midi" example3_midi
  where
    example3_midi = performanceToMidi example3

print3 = runPerf example3
    

  
-- | example 4 - successors and parallel (chord)
example4 :: Performance NrEvent
example4 = perf1 $ 
  root # event (c4 # du4) # chord (map du4 [c4,e4,g4]) 
       # event (d4 # du4) # event (e4 # du4)

run4 :: IO ()
run4 = do
    writeMidi "out/example4.midi" example4_midi
  where
    example4_midi = performanceToMidi example4

print4 = runPerf example4
    
    
-- | example 5 - a performance which is a list of trees
-- | rendered as two staves (or two tracks in midi)
example5 :: Performance NrEvent       
example5 = perf $
    [ root # event (c4 # du4) # event (c4 # du4) # event (d4 # du4) 
           # event (e4 # du4)
    , root # event (c3 # du2) # event (d3 # du2)
    ]

run5 :: IO ()
run5 = do
    writeMidi "out/example5.midi" example5_midi
  where
    example5_midi = performanceToMidi example5  

print5 = runPerf example5
    
-- | example 6 - two staves - top one has chord and a grace
example6 :: Performance NrEvent       
example6 = perf $
    [ root # event (c4 # du4) # a_b_grace # c_triad # event (d4 # du4) 
           # event (e4 # du4)
    , root # event (c3 # du2) # event (d3 # du2)
    ]
  where
    a_b_grace = grace (map du16 [a4, b4])
    c_triad   = chord (map du4 [c4,e4,g4])   

run6 :: IO ()
run6 = do
    writeMidi "out/example6.midi" example6_midi
  where
    example6_midi = performanceToMidi example6  

print6 = runPerf example6

-- example 7 - same staff polyphony - should be rendered as one track in Midi
-- and on the same staff in Abc and LilyPond      
example7 :: Performance NrEvent
example7 = perf1 $ 
  root # poly [ root # event (c5 # du4) # event (c5 # du4) 
                     # event (d5 # du4) # event (e5 # du4)
              , root # event (g4 # du2) # event (e4 # du2) 
              ]
                        
run7 :: IO ()
run7 = do
    writeMidi "out/example7.midi" example7_midi
  where
    example7_midi = performanceToMidi example7

print7 = runPerf example7

main = do { run1; run2; run3; run4; run5; run6; run7 }


                                   