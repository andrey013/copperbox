
-- ghci - 
-- :set -i..:../HNotate:../ZMidi

module Trees where


import HNotate


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



chord = par
grace = prefix

-- | example 1 - simple list of successive notes.  
example1 :: EventList NrEvent
example1 = eventlist $
    [ c4 # du4, c4 # du4, d4 # du4, e4 # du4 ]
                
 
-- | example 2 - simple parallel - a chord             
example2 :: EventList NrEvent
example2 = root # chord (map du1 [c4,e4,g4])


-- | example 3 - successors and grace.
example3 :: EventList NrEvent
example3 = 
    root # event (c4 # du4) # grace (map du16 [a4, b4]) 
         # event (c4 # du4) # event (d4 # du4) # event (e4 # du4)

  
-- | example 4 - successors and parallel (chord)
example4 :: EventList NrEvent
example4 =
    root # event (c4 # du4) # chord (map du4 [c4,e4,g4]) 
         # event (d4 # du4) # event (e4 # du4)
    
    
-- | example 5 - a performance which is a list of trees
-- | rendered as two staves (or two tracks in midi)
example5a :: EventList NrEvent       
example5a = 
    root # event (c4 # du4) # event (c4 # du4) # event (d4 # du4) 
         # event (e4 # du4)
    
example5b :: EventList NrEvent       
example5b = 
    root # event (c3 # du2) # event (d3 # du2)



    
-- | example 6 - two staves - top one has chord and a grace
example6a :: EventList NrEvent       
example6a = 
    root # event (c4 # du4) # a_b_grace # c_triad # event (d4 # du4) 
         # event (e4 # du4)
  where
    a_b_grace = grace (map du16 [a4, b4])
    c_triad   = chord (map du4 [c4,e4,g4])   
       
example6b :: EventList NrEvent       
example6b = 
    root # event (c3 # du2) # event (d3 # du2)






-- example 7 - same staff polyphony - should be rendered as one track in Midi
-- and on the same staff in Abc and LilyPond      
example7 :: EventList NrEvent
example7 =  
  root # poly [ root # event (c5 # du4) # event (c5 # du4) 
                     # event (d5 # du4) # event (e5 # du4)
              , root # event (g4 # du2) # event (e4 # du2) 
              ]
                        
example_sys :: System NrEvent
example_sys = systemL $ 
    [("example1",   example1),
     ("example2",   example2),
     ("example3",   example3),
     ("example4",   example4),
     ("example5a",  example5a),
     ("example5b",  example5b),
     ("example6a",  example6a),
     ("example6b",  example6b),
     ("example7",   example7)]

ly_template   = "templates/ly0-trees.ly"
ly_output     = "out/ly-trees.ly" 

abc_template  = "templates/abc0-trees.abc"
abc_output    = "out/abc-trees.abc"     

main = do
    outputLilyPond example_sys  ly_template   ly_output
    outputAbc      example_sys  abc_template  abc_output

                                   