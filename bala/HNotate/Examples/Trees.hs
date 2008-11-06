
-- ghci - 
-- :set -i../../HNotate

module Examples.Trees where


import HNotate



-- | example 1 - simple list of successive notes.  
example1 :: EventList
example1 = root |# note c4 du4 
                |# note c4 du4
                |# note d4 du4
                |# note e4 du4


-- | example 2 - simple parallel - a chord             
example2 :: EventList
example2 = root |# chord [c4,e4,g4] du4


-- | example 3 - successors and grace.
example3 :: EventList
example3 = 
    root |# note c4 du4 |# gracenotes [(a4,du32), (b4,du32)]
         |# note c4 du4 |# note d4 du4 |# note e4 du4


-- | example 4 - successors and parallel (chord)
example4 :: EventList
example4 =
    root |# note c4 du4 |# chord [c4,e4,g4] du4 
         |# note d4 du4 |# note e4 du4
    

-- | example 5 - a performance which is a list of trees
-- | rendered as two staves (or two tracks in midi)
example5a :: EventList       
example5a = 
    root |# note c4 du4 |# note c4 du4 |# note d4 du4 
         |# note e4 du4
   
example5b :: EventList       
example5b = 
    root |# note c3 du2 |# note d3 du2



    
-- | example 6 - two staves - top one has chord and a grace
example6a :: EventList       
example6a = 
    root |# note c4 du4 |# a_b_grace |# c_triad |# note d4 du4
         |# note e4 du4
  where
    a_b_grace = gracenotes [(a4,du32), (b4,du32)]
    c_triad   = chord [c4,e4,g4] du4   
       
example6b :: EventList       
example6b = 
    root |# note c3 du2 |# note d3 du2






-- example 7 - same staff polyphony - should be rendered as one track in Midi
-- and on the same staff in Abc and LilyPond      
example7 :: EventList
example7 =  
  root |# [ root |# note c5 du4 |# note c5 du4 
                                |# note d5 du4 |# note e5 du4
          , root |# note g4 du2 |# note e4 du2
          ]
                     
example_sys :: System
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

main :: IO ()
main = do
    outputAbc       3 example_sys  abc_template  abc_output
    outputLilyPond  3 example_sys  ly_template   ly_output
    midiTrees

midiTrees :: IO ()
midiTrees = 
    outputMidi upd_env allEventLists example_sys  "./out/trees.mid"
  where 
    upd_env = set_sequential_midi_output 1000
    
                              