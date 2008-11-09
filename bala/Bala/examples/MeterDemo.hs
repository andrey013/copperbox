
-- ghci ...
-- :set -i../../HNotate:../../Bala

module MeterDemo where

import Bala.Base hiding (a4, duration)
import HNotate hiding (a4, duration)

import Data.List
import Data.Ratio

import HNotate.BackendMidi
import Data.Sequence

restS d = (:) (R d)  
attackS d = (:) (N () d)

a4    = attackS du4
r4    = restS du4
a8    = attackS du8
r8    = restS du8
a8'   = attackS (dotn 1 du8)
a16   = attackS du16
r16   = restS du16

type BeatS = [Beat] -> [Beat]

sb_tap :: BeatS
sb_tap  = r16 . a8 . a16 . r16 . a8 . a16 
        . r16 . a8 . a16 . r16 . a16 . a8
sb_foot = r4 . a8 . (tie . a8 .  a4) . a8' . a16

samba_baiao = ([sb_tap $ [], sb_foot $ []], [a5,c4])

tie :: BeatS
tie (N () d : N () d' : xs) = N () (d+d') :xs

metricalSystem :: String -> [[Beat]] -> [Pitch] -> System
metricalSystem name bss ps = system1 name $ root |# xs
  where xs = foldr fn [] (zip bss ps)
        fn (bs,p) xs = (metricalLine $ playmetrical bs p) : xs

metricalLine :: [PitchEvent] -> EventList
metricalLine = foldl' fn root where
    fn evts (N p d) = evts |# note p d
    fn evts (R d)   = evts |# rest d

playmetrical :: [Beat] -> Pitch -> [PitchEvent]
playmetrical bs p = step bs where
  step []         = []
  step (N _ d:bs) = N p d : step bs
  step (R d:bs)   = R d   : step bs


outputMetricalPattarn :: String -> ([[Beat]], [Pitch]) -> FilePath -> IO ()
outputMetricalPattarn name (bss,ps) path = 
    outputMidi id (getEventList name) mp_sys path
  where 
    mp_sys = metricalSystem name bss ps

main :: IO ()
main = do 
    outputMetricalPattarn "samba_baiao" samba_baiao "./out/samba_baiao.mid"


         