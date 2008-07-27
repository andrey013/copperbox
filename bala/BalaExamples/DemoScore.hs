{-# LANGUAGE MultiParamTypeClasses #-}

-- :set -i../


module Main where


import Bala.Base
import Bala.Format.Score
import Bala.Format.Output.LilyPondInternals (unLy)

import Bala.Perform.Bala.BalaLy   -- to get the LilyPond instances
import Bala.Perform.Bala.BalaScore     -- to get the Bala instances
import Bala.Perform.Base.Class
import Bala.Perform.Base.EventTree
import Bala.Perform.LilyPond.LyBackend
import qualified Bala.Perform.LilyPond.LyBackend as Ly
import Bala.Perform.Score.ToScore

import Text.PrettyPrint.Leijen hiding (dot)

main = demo01_ly

---
demo_printtag = putDoc (pretty $ ScTag 45)


r1 = ScRest 0.5
n1 = ScNote (ScPitch c4) 0.5
n2 = ScNote (ScPitch e4) 0.5
n3 = ScNote (ScPitch g4) 0.5

ch1 = ScGroup ScChord [n1,n2,n3]

score1 = root # event (c4 # du4) # event (d4 # du4) # event (e4 # du4)


simpledoc :: Pretty a => a -> SimpleDoc
simpledoc e = renderPretty 0.8 80 (pretty e)

printDoc :: Pretty a => a -> IO ()
printDoc e = putStr ((displayS (simpledoc e) []) ++ "\n")


showScore sc = printDoc $ pretty $ toScore1 sc default_score_env
showScoreP sc = printDoc $ pretty $ toScore sc default_score_env

toLy et = let sc = toScore1 et default_score_env
          in Ly.generateLilyPondScore sc Ly.default_ly_env


demo_s1 = showScore score1


demo01 = showScore example1
demo01_ly = printDoc $ unLy $ toLy example1

demo02 = showScore example2

demo03 = showScore example3

demo04 = showScore example4

demo04a = showScore (example4 # event (c4 # du4) # event (c4 # du4))

demo05 = showScoreP example5

demo06 = showScoreP example6

demo07 = showScore example7

demo07a = showScore example7a 

--------------------------------------------------------------------------------

data NrEvent = Note Pitch Duration
             | Rest Duration
  deriving (Eq,Show)

du1,du2,du4,du8,du16 :: Pitch -> NrEvent
du1 p    = Note p whole
du2 p    = Note p half
du4 p    = Note p quarter
du8 p    = Note p eighth
du16 p   = Note p sixteenth



instance Perform NrEvent Pitch Duration where
  eventvalues (Note p d) = (Just p, Just d)
  eventvalues (Rest d)   = (Nothing, Just d)

-- | example 1 - simple list of successive notes.  
example1 :: EventTree NrEvent
example1 = root # event (c4 # du4) # event (c4 # du4) # event (d4 # du4) 
                # event (e4 # du4)
                
    
-- | example 2 - simple parallel - a chord             
example2 :: EventTree NrEvent
example2 = root # chord (map du1 [c4,e4,g4])

    
-- | example 3 - successors and grace.
example3 :: EventTree NrEvent
example3 = root # event (c4 # du4) # grace (map du16 [a4, b4]) 
                # event (c4 # du4) # event (d4 # du4) # event (e4 # du4)

    
-- | example 4 - successors and parallel (chord)
example4 :: EventTree NrEvent
example4 = root # event (c4 # du4) # chord (map du4 [c4,e4,g4]) 
                # event (d4 # du4) # event (e4 # du4)
    
    
-- | example 5 - a performance which is a list of trees
-- | rendered as two staves (or two tracks in midi)
example5 :: Performance NrEvent       
example5 = perf $
    [ root # event (c4 # du4) # event (c4 # du4) # event (d4 # du4) 
           # event (e4 # du4)
    , root # event (c3 # du2) # event (d3 # du2)
    ]
    
    
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


-- example 7 - same staff polyphony - should be rendered as one track in Midi
-- and on the same staff in Abc and LilyPond      
example7 :: EventTree NrEvent
example7 = root # poly [ root # event (c5 # du4) # event (c5 # du4) 
                              # event (d5 # du4) # event (e5 # du4)
                       , root # event (g4 # du2) # event (e4 # du2) 
                       ]
                

----

example7a = root  # event (c3 # du2) 
                  # poly [ root # event (c5 # du4) # event (c5 # du4) 
                                # event (d5 # du4) # event (e5 # du4)
                         , root # event (g4 # du2) # event (e4 # du2) 
                         ] 
                  # event (c3 # du2) 
                  # event (c3 # du2)     
                                  