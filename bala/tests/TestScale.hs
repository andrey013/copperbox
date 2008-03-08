

module TestScale where

import Bala

import Test.QuickCheck

prop_WellFormed = octaveComplete


middle_c :: Pitch
middle_c = (read "C4")
c4 = middle_c

e4, fs4 :: Pitch 
e4 = read "E4"
fs4 = read "F#4"


t1 = scanl (addSemi) e4 [5,5,5,4,5]
