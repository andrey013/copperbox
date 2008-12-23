{-# LANGUAGE FlexibleInstances #-}


-- Haskore from this darcs repository:
-- darcs get http://cathay.cs.yale.edu/Haskore

-- :set -i../HNotateHaskore:../../HNotate:./examples

module HaskoreHnDemo where

import SelfSim (sim1)
import Ssf (ssf)
import ChildSong6 (childSong6)

import HaskoreHNotate
import TranslateMusic
import HNotate hiding (Pitch(..))

import Haskore


import Data.Ratio
import Data.Sequence


t251' :: Music Pitch
t251' = t251 

t251_sys = psystem $  makePerf $ t251'

t251_ly_template = "templates/ly0-t251.ly"
t251_ly_output   = "out/ly-t251.ly"

demo1 = do 
    outputLilyPond DebugOn t251_sys  t251_ly_template t251_ly_output

 
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

ssf' :: Music Pitch
ssf' = ssf


sys_ssf = psystem (makePerf ssf) 


-- This one fails - it has an unrenderable duration... 
demo2 = do 
    outputLilyPond DebugOn sys_ssf  ssf_ly_template   ssf_ly_output

    
sim1_ly_template   = "templates/ly0-sim1.ly"
sim1_ly_output     = "out/ly-sim1.ly"

sim1' :: (Num t) => t -> Music Pitch
sim1' = sim1

sys_sim1 = psystem (makePerf $ sim1 3) 


demo3 = do 
    outputLilyPond DebugOn sys_sim1  sim1_ly_template   sim1_ly_output

    

-- midi = perf defPMap defCon m (defPMap = fancyPlayer)

class MakePerformance a where
  makePerf :: a -> Performance

instance MakePerformance (Music Pitch) where
  makePerf m = perform defPMap defCon (fmap (\p -> (p,[])) m) 
  
instance MakePerformance (Music (Pitch, [NoteAttribute])) where
  makePerf m = perform defPMap defCon m         




          