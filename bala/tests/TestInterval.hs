
module TestInterval 
  ( testInterval
  ) where

import ArbitraryInstances
import Bala

import Test.QuickCheck


nthPred x i | i > 0 =  nthPred (pred x) (i-1)
            | otherwise = x

nthSucc x i | i > 0     = nthSucc (succ x) (i-1)
            | otherwise = x 



prop_Unison,prop_Octave :: Pitch -> Bool          
prop_Unison a = arithmeticDistance a a == 1 
prop_Octave a = arithmeticDistance a (a `ove` 1) == 8

testInterval = mapM_ quickCheck [prop_Unison, prop_Octave]

prop_minus_p1 :: Interval -> Bool
prop_minus_p1 a = a - a == perfect_unison

prop_minus2_neg :: Interval -> Bool
prop_minus2_neg a = a - a - a == negate a

p5 = minor_third + major_third

prop_minus_neg :: Int -> Int -> Bool
prop_minus_neg a b = a - b == negate (a+b)


-- c_to_c'sharp = arithmetic distance 8, 13 semitones
c_to_c'sharp :: Interval
c_to_c'sharp = fromInteger 13

-- c_to_c'sharp = arithmetic distance9, 14 semitones
c_to_d' :: Interval
c_to_d' = fromInteger 14

d'_to_c :: Interval
d'_to_c = fromInteger (-14)


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

