{-# LANGUAGE FlexibleInstances #-}

-- :set -i../HNotateHaskore:../../HNotate:./examples
module HaskoreHnDemo where

import SelfSim (sim1)
import Ssf (ssf)
import ChildSong6 (childSong6)

import HNotateHaskore
import HNotate hiding (Pitch(..))

import Haskore


import Data.Ratio
import Data.Sequence


-- demo = psq $ makePerf $ sim1 3

t251_sys = psystem $  makePerf $ t251

t251_ly_template = "templates/ly0-t251.ly"
t251_ly_output   = "out/ly-t251.ly"

demo = do 
    outputLilyPond 5 t251_sys  t251_ly_template t251_ly_output

 
middle'c = c 5 hn


  
-- mc:: NrEvent 
mc = line [middle'c]


intR :: Rational -> Double
intR r = 
    let (n,d) = (fromIntegral $ numerator r, fromIntegral $ denominator r) 
    in  n /d

foldTimes :: Performance -> DurT
foldTimes = foldr (\(Event {eDur=d}) a -> a+d) (0%1) 

scanTimes = scanl (\a (Event {eDur=d}) -> a+d) (0%1)

dur_dotted_whole :: Duration
dur_dotted_whole = convert dwn

ssf_ly_template   = "templates/ly0-ssf.ly"
ssf_ly_output     = "out/ly-ssf.ly"

sys_ssf = psystem (makePerf ssf) 

{-
-- Warning don't run this - for some reason it sends GHC of into orbit 
demo2 = do 
    outputLilyPond 5 sys_ssf  ssf_ly_template   ssf_ly_output
-}
    
sim1_ly_template   = "templates/ly0-sim1.ly"
sim1_ly_output     = "out/ly-sim1.ly"

sys_sim1 = psystem (makePerf $ sim1 3) 


demo3 = do 
    outputLilyPond 5 sys_sim1  sim1_ly_template   sim1_ly_output

    

-- midi = perf defPMap defCon m (defPMap = fancyPlayer)

class MakePerformance a where
  makePerf :: a -> Performance

instance MakePerformance (Music Pitch) where
  makePerf m = perform defPMap defCon (fmap (\p -> (p,[])) m) 
  
instance MakePerformance (Music (Pitch, [NoteAttribute])) where
  makePerf m = perform defPMap defCon m         




          