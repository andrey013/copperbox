
-- ghci ...
-- :set -i../../HNotate:../../Bala

-- Its easy to output 1 scale with HNotate, but how do we output many scales?
-- Do we need the doc combinators back - uh oh? 


module ScaleDemo where

import Bala.Base hiding (a_major)
import HNotate

import Data.List

a_major :: Scale
a_major = makeScale a4 major_intervals

data MetricalEvent a = N a Duration | R Duration
  deriving (Eq,Show)

type Beat = MetricalEvent ()
type PitchEvent = MetricalEvent Pitch

beats :: Duration -> [Beat] 
beats d = repeat (N () d)


playScale :: Scale -> [PitchEvent]
playScale  = (playnotes `flip` beats du1) . scaleNotes
 
playnotes :: [Pitch] -> [Beat] -> [PitchEvent]
playnotes ps bs = step ps bs where
    step []     _           = []
    step (p:ps) (N _ d:bs)  = N p d : step ps bs
    step ps     (R d:bs)    = R d   : step ps bs
    step ps     []          = error "playnotes - empty beat list"
  
pitchEventSystem :: String -> [PitchEvent] -> System
pitchEventSystem name = system1 name . foldl' fn root where
    fn evts (N p d) = evts |# note p d
    fn evts (R d)   = evts |# rest d

outputScale :: String -> Scale -> IO ()
outputScale name sc = 
    outputMidi id (getEventList name) scale_sys  "./out/scale.mid"
  where 
    scale_sys = pitchEventSystem name (playScale sc)
  
main :: IO ()
main = outputScale "a_major" a_major 

    