
module TestInterval where

import Bala

iq01 = semitoneDistance (read "C4") (read "C5")

b4 = unMidi (fromPitch b4')
  where b4' :: Pitch
        b4' = read "B4"
        
c4 = unMidi (fromPitch c4')
  where c4' :: Pitch
        c4' = read "C4"
        
f4 = unMidi (fromPitch f4')
  where f4' :: Pitch
        f4' = read "F4"

a4 = unMidi (fromPitch a4')
  where a4' :: Pitch
        a4' = read "A4"
                
a5 = unMidi (fromPitch a5')
  where a5' :: Pitch
        a5' = read "A5"

-- o01 = countingDistance' F F
-- o02 = countingDistance  F F 

uni1 = mspan (read "F4") (read "F4")

sec1 = mspan (read "F4") (read "G4")

thi1 = mspan (read "F4") (read "A4")

fou1 = mspan (read "F4") (read "B4")

fif1 = mspan (read "F4") (read "C5")

six1 = mspan (read "F4") (read "D5")

sev1 = mspan (read "F4") (read "E5")

oct1 = mspan (read "F4") (read "F5") 

