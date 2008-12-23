
-- ghci ...
-- :set -i../../HNotate:../../Bala


module ScaleDemo where

import Bala.Base
import Bala.MusicRep

import qualified HNotate as H
import HNotate ( system1, root, ( # ) )

import Data.List

demo :: Int
demo = displacement (PitchLabel A Sharp) (PitchLabel B Nat)

a_major :: Scale
a_major = makeScale a4 major_interval_pattern

a_flat_major :: Scale 
a_flat_major = makeScale aes4 major_interval_pattern

data Beat = N Duration | R Duration

beats :: Duration -> [Beat]
beats d = repeat (N d)

playScale :: Scale -> [Event]
playScale  = (playnotes `flip` beats du1) . scaleNotes
 
playnotes :: [Pitch] -> [Beat] -> [Event]
playnotes ps bs = step ps bs where
    step []     _         = []
    step (p:ps) (N d :bs) = note p d : step ps bs
    step ps     (R d :bs) = rest d   : step ps bs
    step ps     []        = error "playnotes - empty beat list"

{-
-- TO DO - use ZMidi...
  
pitchEventSystem :: String -> [Event] -> H.System
pitchEventSystem name = system1 name . foldl' fn root where
    fn evts (Note p d)        = evts # H.note p d
    fn evts (Rest d)          = evts # H.rest d

outputScale :: String -> Scale -> FilePath -> IO ()
outputScale name sc path = 
    H.outputMidi id (H.getEventList name) scale_sys path
  where 
    scale_sys = pitchEventSystem name (playScale sc)
  
main :: IO ()
main = do 
    outputScale "a_major"      a_major      "./out/a_major_scale.mid"
    outputScale "a_flat_major" a_flat_major "./out/a_flat_major_scale.mid"

-}    