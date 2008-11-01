

-- :set -i..:../HNotate:../ZMidi


module Main where


import Bala.Base
import HNotate


import Text.PrettyPrint.Leijen hiding (dot)
import qualified Data.Sequence as S



-- main = demo01_ly
main = print $ system1 "demo" $ foldl (#) root (replicate 12 (note c4 du4))
  where compR op f a b = a `op` f b
  
  
---


score1 = system1 "score1" $ root # note c4 du4 # note d4 du4 # note e4 du4

{-
simpledoc :: Pretty a => a -> SimpleDoc
simpledoc e = renderPretty 0.8 80 (pretty e)

printDoc :: Pretty a => a -> IO ()
printDoc e = putStr ((displayS (simpledoc e) []) ++ "\n")


showScore sc = printDoc $ pretty $ toScore sc default_score_env
showScoreP sc = printDoc $ pretty $ toScore sc default_score_env


toLy perf = let sc0   = toScore perf default_score_env
                lysc  = lyscore sc0
            in generateLilyPondScore lysc default_ly_env

toAbc perf = let sc0    = toScore perf default_score_env
                 abcsc  = abcscore sc0
            in generateAbcScore abcsc default_abc_env

createDoc un = vsep . map (pretty . un)


demo_s1 = showScore score1
-}

{-
printLilyPond t a = printLy $ systemToLy (default_ly_system t pre) a
  where pre = elementStart +++ key _c major +++ clef treble
-}  

{-
demo01      = showScore example1
demo01_ly   = printLilyPond "example1" example1
-- demo01_abc  = printDoc $ createDoc getAbc $ toAbc example1

demo02 = showScore example2
demo02_ly = printLilyPond "example2" example2

demo03 = showScore example3
demo03_ly = printLilyPond "example3" example3

demo03a_ly = printLilyPond "example3a"  example3a


demo04 = showScore example4
demo04_ly = printLilyPond "example4" example4


demo05 = showScoreP example5
demo05_ly = printLilyPond "example5" example5

demo06 = showScoreP example6
demo06_ly = printLilyPond "example6" example6

demo07 = showScore example7
demo07_ly = printLilyPond "example7" example7

demo07a = showScore example7a 
demo07a_ly = printLilyPond "example7a" example7a

demo07b = showScore example7b 
demo07b_ly = printLilyPond "example7b" example7b

demo07c = showScore example7c
demo07c_ly = printLilyPond "example7c" example7c


demo07d = showScore example7d
demo07d_ly = printLilyPond "example7d" example7d

-}

--------------------------------------------------------------------------------


-- | example 1 - simple list of successive notes.  
example1 :: System
example1 = system1 "example1" $ 
  root # note c4 du4 # note c4 du4 # note d4 du4 
       # note e4 du4
                
    
-- | example 2 - simple parallel - a chord             
example2 :: System
example2 = system1 "example2" $ 
  root # chord [c4,e4,g4] du1

    
-- | example 3 - successors and gracenotes.
example3 :: System
example3 = system1 "example3" $ 
  root # note c4 du4 # gracenotes [a4, b4]
       # note c4 du4 # note d4 du4 # note e4 du4

example3a :: System
example3a = system1 "example3a" $ 
  root # note c4 du4 # gracenotes [a4, b4] # note c4 du4
                

    
-- | example 4 - successors and parallel (chord)
example4 :: System
example4 = system1 "example4" $ 
  root # note c4 du4 # chord [c4,e4,g4] du4 
                # note d4 du4 # note e4 du4
    
    
-- | example 5 - a performance which is a list of trees
-- | rendered as two staves (or two tracks in midi)
example5 :: System       
example5 = systemL $ 
    [ ("example5_part1", 
        root # note c4 du4 # note c4 du4 # note d4 du4 
             # note e4 du4)
    , ("example5_part2", 
        root # note c3 du2 # note d3 du2)
    ]
    
    
-- | example 6 - two staves - top one has chord and gracenotes
example6 :: System       
example6 = systemL $ 
    [ ("example6_part1", 
        root # note c4 du4 # a_b_grace # c_triad # note d4 du4 
             # note e4 du4)
    , ("example6_part2", 
        root # note c3 du2 # note d3 du2)
    ]
  where
    a_b_grace = gracenotes [a4, b4]
    c_triad   = chord [c4,e4,g4] du4   


-- example 7 - same staff polyphony - should be rendered as one track in Midi
-- and on the same staff in Abc and LilyPond      
example7 :: System
example7 = system1 "example7" $ 
  root # poly [ root # note c5 du4 # note c5 du4 
                     # note d5 du4 # note e5 du4
              , root # note g4 du2 # note e4 du2 
              ]
                
-- c5/4 c5/4 d5/4 e5/4 ----
-- g4/2 ---- e4/2 ---- ---- 


----

example7a :: System
example7a = system1 "example7a" $
  root  # note c4 du2 
        # poly [ root # note c5 du4 # note c5 du4 
                      # note d5 du4 # note e5 du4
               , root # note g4 du2 # note e4 du2 
               ] 
        # note c4  du2 
        # note c4  du2  

-- Do we want this:
 
-- c4/2 ---- ---- ---- ---- c4/2 ---- c4/2 ----
-- ---- c5/4 c5/4 d5/4 e5/4 ---- 
-- ---- g4/2 ---- e4/2 ---- ---- 

-- Or this:

-- c4/2 ---- c4/2 ---- c4/2 ----
-- ---- c5/4 c5/4 d5/4 e5/4 ---- 
-- ---- g4/2 ---- e4/2 ---- ---- 



-- poly optimization         
example7b = system1 "example7b" $ 
  root # note c4 du4 # poly [ root # note d4 du4 ] 
       # note e4 du4 # note f4 du4 
       
-- c4/4 d4/4 e4/4 f4/4                 

-- poly nesting
example7c = system1 "example7c" $
  root # poly [ root # note c4 du1
              , root # note e4 du2 
                     # poly [ root # note f4 du2
                            , root # note g4 du2
                            ]
              ]
              
-- c4/1 ---- ---- ---- ----
-- e4/2 ---- ---- ---- ----             
-- ---- f4/2 ---- ---- ----
-- ---- g4/2 ---- ---- ----

example7d :: System
example7d = system1 "example7d" $
  root  # note c3  du2 # note c3  du2 
        # note d3  du2 # note e3  du2
        # poly [ root # note c5  du4 # note c5  du4 
                      # note d5  du4 # note e5  du4
               , root # note g4  du2 # note e4  du2 
               ] 

-- c3/2 ---- c3/2 ---- d3/2 ---- e3/2 ---- ---- ---- ---- ----
-- ---- ---- ---- ---- ---- ---- ---- c5/4 c5/4 d5/4 e5/4 ----
-- ---- ---- ---- ---- ---- ---- ---- g4/2 ---- e4/2 ---- ----



                       