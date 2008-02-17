
module TestInterval where

import ArbitraryInstances
import Bala

import Control.Applicative
import Test.QuickCheck

        
                
prop_Unison a = arithmeticDistance a a == 1 
prop_Octave a = arithmeticDistance a (a `ove` 1) == 8

main = mapM_ quickCheck [prop_Unison, prop_Octave]


iq01 = semitoneDistance (read "C4") (read "C5")

b4,c4,f4,a4,a5 :: Pitch
b4 = read "B4"
c4 = read "C4"
f4 = read "F4"
a4 = read "A4"
a5 = read "A5"

-- o01 = countingDistance' F F
-- o02 = countingDistance  F F 

{-

uni1 = mspan (read "F4") (read "F4")

sec1 = mspan (read "F4") (read "G4")

thi1 = mspan (read "F4") (read "A4")

fou1 = mspan (read "F4") (read "B4")

fif1 = mspan (read "F4") (read "C5")

six1 = mspan (read "F4") (read "D5")

sev1 = mspan (read "F4") (read "E5")

oct1 = mspan (read "F4") (read "F5") 

-}
