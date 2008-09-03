
-- ghci - 
-- :set -i..:../HNotate:../ZMidi

module Trees where

import Bala.Base hiding (toPitch)
import qualified Bala.Base as B
import HNotate
import HNotate.Print.OutputLilyPond hiding (chord, grace)

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



instance Event NrEvent where
  eventvalues (Note p d) = (Just $ renderPitch p, Just $ renderDuration d)
  eventvalues (Rest d)   = (Nothing, Just $ renderDuration d)






-- | example 1 - simple list of successive notes.  
example1 :: System NrEvent
example1 = system1 $ 
    root # event (c4 # du4) # event (c4 # du4) # event (d4 # du4) 
         # event (e4 # du4)


                

run1 :: IO ()
run1 = do
    writeMidi "out/example1.midi" example1_midi
  where
    example1_midi = systemToMidi default_midi_system example1
    
print1 = printMidi $ systemToMidi default_midi_system example1 

print1_ly = printLy $ systemToLy (default_ly_system "example1" pre) example1
  where pre = elementStart +++ key _c major +++ clef treble

sc1 = pretty $ toScore example1 default_score_env
   
-- | example 2 - simple parallel - a chord             
example2 :: System NrEvent
example2 = system1 $ root # chord (map du1 [c4,e4,g4])

run2 :: IO ()
run2 = do
    writeMidi "out/example2.midi" example2_midi
  where
    example2_midi = systemToMidi default_midi_system example2

print2 = printMidi $ systemToMidi default_midi_system example2

sc2 = pretty $ toScore example2 default_score_env
    
-- | example 3 - successors and grace.
example3 :: System NrEvent
example3 = system1 $
  root # event (c4 # du4) # grace (map du16 [a4, b4]) 
       # event (c4 # du4) # event (d4 # du4) # event (e4 # du4)

run3 :: IO ()
run3 = do
    writeMidi "out/example3.midi" example3_midi
  where
    example3_midi = systemToMidi default_midi_system example3

print3 = printMidi $ systemToMidi default_midi_system example3
    

  
-- | example 4 - successors and parallel (chord)
example4 :: System NrEvent
example4 = system1 $ 
  root # event (c4 # du4) # chord (map du4 [c4,e4,g4]) 
       # event (d4 # du4) # event (e4 # du4)

run4 :: IO ()
run4 = do
    writeMidi "out/example4.midi" example4_midi
  where
    example4_midi = systemToMidi default_midi_system example4

print4 = printMidi $ systemToMidi default_midi_system example4
    
    
-- | example 5 - a performance which is a list of trees
-- | rendered as two staves (or two tracks in midi)
example5 :: System NrEvent       
example5 = system $
    [ root # event (c4 # du4) # event (c4 # du4) # event (d4 # du4) 
           # event (e4 # du4)
    , root # event (c3 # du2) # event (d3 # du2)
    ]

run5 :: IO ()
run5 = do
    writeMidi "out/example5.midi" example5_midi
  where
    example5_midi = systemToMidi default_midi_system example5  

print5 = printMidi $ systemToMidi default_midi_system example5
    
-- | example 6 - two staves - top one has chord and a grace
example6 :: System NrEvent       
example6 = system $
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
    example6_midi = systemToMidi default_midi_system example6  

print6 = printMidi $ systemToMidi default_midi_system example6

-- example 7 - same staff polyphony - should be rendered as one track in Midi
-- and on the same staff in Abc and LilyPond      
example7 :: System NrEvent
example7 = system1 $ 
  root # poly [ root # event (c5 # du4) # event (c5 # du4) 
                     # event (d5 # du4) # event (e5 # du4)
              , root # event (g4 # du2) # event (e4 # du2) 
              ]
                        
run7 :: IO ()
run7 = do
    writeMidi "out/example7.midi" example7_midi
  where
    example7_midi = systemToMidi default_midi_system example7

print7 = printMidi $ systemToMidi default_midi_system example7

main = do { run1; run2; run3; run4; run5; run6; run7 }


                                   