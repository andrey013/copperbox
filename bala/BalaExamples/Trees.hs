

module Trees where

import Bala.Base
import Bala.Perform.EventTree
import Bala.Perform.PerformBase
import Bala.Perform.PerformMidi

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
  opitch (Note p _) = Just p
  opitch (Rest _)   = Nothing
  
  oduration (Note _ d) = Just d
  oduration (Rest d)   = Just d

-- | example 1 - simple list of successive notes.  
example1 :: EventTree NrEvent
example1 = root # event (c4 # du4) # event (c4 # du4) # event (d4 # du4) 
                # event (e4 # du4)

run1 :: IO ()
run1 = do
    writeMidi "out/example1.midi" example1_midi
  where
    example1_midi = renderMidi1 example1 default_midi_env
    
-- | example 2 - simple parallel - a chord             
example2 :: EventTree NrEvent
example2 = root # chord (map du1 [c4,e4,g4])

run2 :: IO ()
run2 = do
    writeMidi "out/example2.midi" example2_midi
  where
    example2_midi = renderMidi1 example2 default_midi_env
    
-- | example 3 - successors and grace.
example3 :: EventTree NrEvent
example3 = root # event (c4 # du4) # grace (map du16 [a4, b4]) 
                # event (c4 # du4) # event (d4 # du4) # event (e4 # du4)

run3 :: IO ()
run3 = do
    writeMidi "out/example3.midi" example3_midi
  where
    example3_midi = renderMidi1 example3 default_midi_env
    
-- | example 4 - successors and parallel (chord)
example4 :: EventTree NrEvent
example4 = root # event (c4 # du4) # chord (map du4 [c4,e4,g4]) 
                # event (d4 # du4) # event (e4 # du4)

run4 :: IO ()
run4 = do
    writeMidi "out/example4.midi" example4_midi
  where
    example4_midi = renderMidi1 example4 default_midi_env
    
    
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
    example5_midi = renderMidi example5 default_midi_env  
    
    
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
    example6_midi = renderMidi example6 default_midi_env  


-- example 7 - same staff parallel - should be rendered as one track in Midi
-- and on the same staff in Abc and LilyPond      
example7 :: EventTree NrEvent
example7 = root # parallel [ root # event (c5 # du4) # event (c5 # du4) 
                                  # event (d5 # du4) # event (e5 # du4)
                           , root # event (g4 # du2) # event (e4 # du2) 
                           ]
                        
run7 :: IO ()
run7 = do
    writeMidi "out/example7.midi" example7_midi
  where
    example7_midi = renderMidi1 example7 default_midi_env


main = do { run1; run2; run3; run4; run5; run6; run7 }
                                   