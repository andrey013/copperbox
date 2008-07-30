{-# LANGUAGE MultiParamTypeClasses #-}

-- :set -i../


module Main where


import Bala.Base

import Bala.Format.Output.LilyPondInternals (unLy)
import Bala.Format.Score


import Bala.Perform.Bala.BalaLy   -- to get the LilyPond instances
import Bala.Perform.Bala.BalaScore     -- to get the Bala instances
import Bala.Perform.Base.Class
import Bala.Perform.Base.EventTree
import Bala.Perform.LilyPond.LyBackend
import qualified Bala.Perform.LilyPond.LyBackend as Ly
import Bala.Perform.Score.ToPolyScore
import Bala.Perform.Score.ToScore

import Text.PrettyPrint.Leijen hiding (dot)

---
import Bala.Perform.Base.OnsetQueue
import Bala.Format.Score.PolyDatatypes
import qualified Data.Sequence as S

data Pair = Pair Int Char 
instance OnsetEvent Pair Char where
  onset (Pair i c) = (i,c)

oq1 = buildQueue [Pair 1 'a', Pair 2 'b', Pair 2 'B', Pair 3 'c']

mkq sc = let (ScScore sq) = toScore sc default_score_env
         in fmap fn $ deriveQueue $ S.index sq 0
  where
    fn (PScMeasure i v s) = if (S.null s) 
                              then show v ++ " :" ++ show i ++ "  ><  " 
                              else show v ++ " :" ++ show i ++ " ---- "

trans sc = printDoc $ pretty $ score $ toScore sc default_score_env

-- main = demo01_ly
main = showScore $ perf1 $ foldl (compR (#) event) root (replicate 12 (c4 # du4))
  where compR op f a b = a `op` f b
  
  
---

r1 = ScRest 0.5
n1 = ScNote (ScPitch c4) 0.5
n2 = ScNote (ScPitch e4) 0.5
n3 = ScNote (ScPitch g4) 0.5

ch1 = ScGroup ScChord [n1,n2,n3]

score1 = perf1 $ root # event (c4 # du4) # event (d4 # du4) # event (e4 # du4)


simpledoc :: Pretty a => a -> SimpleDoc
simpledoc e = renderPretty 0.8 80 (pretty e)

printDoc :: Pretty a => a -> IO ()
printDoc e = putStr ((displayS (simpledoc e) []) ++ "\n")


showScore sc = printDoc $ pretty $ toScore sc default_score_env
showScoreP sc = printDoc $ pretty $ toScore sc default_score_env


toLy perf = let sc = toScore perf default_score_env
            in Ly.generateLilyPondScore sc Ly.default_ly_env

createDoc = vsep . map (pretty . unLy)


demo_s1 = showScore score1


demo01 = showScore example1
demo01_ly = printDoc $ createDoc $ toLy example1

demo02 = showScore example2
demo02_ly = printDoc $ createDoc $ toLy example2

demo03 = showScore example3
demo03_ly = printDoc $ createDoc $ toLy example3

demo03a_ly = printDoc $ createDoc $ toLy example3a


demo04 = showScore example4
demo04_ly = printDoc $ createDoc $ toLy example4


demo05 = showScoreP example5
demo05_ly = printDoc $ createDoc $ toLy example5

demo06 = showScoreP example6
demo06_ly = printDoc $ createDoc $ toLy example6

demo07 = showScore example7
demo07_ly = printDoc $ createDoc $ toLy example7

demo07a = showScore example7a 
demo07a_ly = printDoc $ createDoc $ toLy example7a

demo07b = showScore example7b 
demo07b_ly = printDoc $ createDoc $ toLy example7b

demo07c = showScore example7c
demo07c_ly = printDoc $ createDoc $ toLy example7c


demo07d = showScore example7d
demo07d_ly = printDoc $ createDoc $ toLy example7d


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
example1 :: Performance NrEvent
example1 = perf1 $ 
  root # event (c4 # du4) # event (c4 # du4) # event (d4 # du4) 
       # event (e4 # du4)
                
    
-- | example 2 - simple parallel - a chord             
example2 :: Performance NrEvent
example2 = perf1 $ 
  root # chord (map du1 [c4,e4,g4])

    
-- | example 3 - successors and grace.
example3 :: Performance NrEvent
example3 = perf1 $ 
  root # event (c4 # du4) # grace (map du16 [a4, b4]) 
       # event (c4 # du4) # event (d4 # du4) # event (e4 # du4)

example3a :: Performance NrEvent
example3a = perf1 $ 
  root # event (c4 # du4) # grace (map du16 [a4, b4]) # event (c4 # du4)
                

    
-- | example 4 - successors and parallel (chord)
example4 :: Performance NrEvent
example4 = perf1 $ 
  root # event (c4 # du4) # chord (map du4 [c4,e4,g4]) 
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
example7 :: Performance NrEvent
example7 = perf1 $ 
  root # poly [ root # event (c5 # du4) # event (c5 # du4) 
                     # event (d5 # du4) # event (e5 # du4)
              , root # event (g4 # du2) # event (e4 # du2) 
              ]
                
-- c5/4 c5/4 d5/4 e5/4 ----
-- g4/2 ---- e4/2 ---- ---- 


----

example7a :: Performance NrEvent
example7a = perf1 $
  root  # event (c4 # du2) 
        # poly [ root # event (c5 # du4) # event (c5 # du4) 
                      # event (d5 # du4) # event (e5 # du4)
               , root # event (g4 # du2) # event (e4 # du2) 
               ] 
        # event (c4 # du2) 
        # event (c4 # du2)  

-- Do we want this:
 
-- c4/2 ---- ---- ---- ---- c4/2 ---- c4/2 ----
-- ---- c5/4 c5/4 d5/4 e5/4 ---- 
-- ---- g4/2 ---- e4/2 ---- ---- 

-- Or this:

-- c4/2 ---- c4/2 ---- c4/2 ----
-- ---- c5/4 c5/4 d5/4 e5/4 ---- 
-- ---- g4/2 ---- e4/2 ---- ---- 



-- poly optimization         
example7b = perf1 $ 
  root # event (c4 # du4) # poly [ root # event (d4 # du4) ] 
       # event (e4 # du4) # event (f4 # du4) 
       
-- c4/4 d4/4 e4/4 f4/4                 

-- poly nesting
example7c = perf1 $
  root # poly [ root # event (c4 # du1) 
              , root # event (e4 # du2) 
                     # poly [ root # event (f4 # du2)
                            , root # event (g4 # du2)
                            ]
              ]
              
-- c4/1 ---- ---- ---- ----
-- e4/2 ---- ---- ---- ----             
-- ---- f4/2 ---- ---- ----
-- ---- g4/2 ---- ---- ----

example7d :: Performance NrEvent
example7d = perf1 $
  root  # event (c3 # du2) # event (c3 # du2) 
        # event (d3 # du2) # event (e3 # du2)
        # poly [ root # event (c5 # du4) # event (c5 # du4) 
                      # event (d5 # du4) # event (e5 # du4)
               , root # event (g4 # du2) # event (e4 # du2) 
               ] 

-- c3/2 ---- c3/2 ---- d3/2 ---- e3/2 ---- ---- ---- ---- ----
-- ---- ---- ---- ---- ---- ---- ---- c5/4 c5/4 d5/4 e5/4 ----
-- ---- ---- ---- ---- ---- ---- ---- g4/2 ---- e4/2 ---- ----



                       