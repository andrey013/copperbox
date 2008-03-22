
module TestInterval 
  ( testInterval
  ) where

import ArbitraryInstances
import Bala

import Test.QuickCheck

        
prop_Unison,prop_Octave :: Pitch -> Bool          
prop_Unison a = arithmeticDistance a a == 1 
prop_Octave a = arithmeticDistance a (a `ove` 1) == 8

testInterval = mapM_ quickCheck [prop_Unison, prop_Octave]

-- interval addition (Duckworth):
-- M3 + m3 = P5
-- m3 + M3 = P5
-- M3 + M3 = A5
-- m3 + m3 = d5




{-

iq01 = semitoneDistance (read "C4") (read "C5")

b4,c4,f4,a4,a5 :: Pitch
b4 = read "B4"
c4 = read "C4"
f4 = read "F4"
a4 = read "A4"
a5 = read "A5"

-}


-- dominant7 :: ScaleDegreePattern
-- dominant7 = decouper "1 3 5 b7"

