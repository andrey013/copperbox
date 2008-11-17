
-- ghci ...
-- :set -i../../HNotate:../../Bala

-- Its easy to output 1 scale with HNotate, but how do we output many scales?
-- Do we need the doc combinators back - uh oh? 


module ScaleDemo where

import Bala.Base
import Bala.MusicRep

import HNotate

import Data.List

demo :: Int
demo = displacement (PitchLabel A Sharp) (PitchLabel B Nat)

a_major :: Scale
a_major = makeScale a4 major_interval_pattern

a_flat_major :: Scale 
a_flat_major = makeScale aes4 major_interval_pattern




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

outputScale :: String -> Scale -> FilePath -> IO ()
outputScale name sc path = 
    outputMidi id (getEventList name) scale_sys path
  where 
    scale_sys = pitchEventSystem name (playScale sc)
  
main :: IO ()
main = do 
    outputScale "a_major" a_major "./out/a_major_scale.mid"
    outputScale "a_flat_major" a_flat_major "./out/a_flat_major_scale.mid"

    