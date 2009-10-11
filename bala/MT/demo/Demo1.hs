{-# OPTIONS -Wall #-}

module Demo1 where

import PLRGroup
import TIGroup
import Z12

t1 :: Z12
t1 = transpose (-4) 7

i1, i2 :: Z12
i1 = invert 0
i2 = invert 7



ls1 :: [Z12]
ls1 = invert [0,7]


trnsp_triad :: [Z12]
trnsp_triad = transpose 1 [0,4,7]

inv_triad :: [Z12]
inv_triad = invert [0,4,7]


p1 :: (Z12,Z12,Z12)
p1 = nrP (0,4,7)

l1 :: Triad 
l1 = nrL (0,4,7)

r1 :: Triad
r1 = nrR (0,4,7)

_R :: Int
_R = 10

