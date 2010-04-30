

module Triadic where

import UTT.Base
import UTT.TCBase

test01 :: Bool
test01 = invert dominant == UTT Pos 7 7

test02 :: Bool 
test02 = invert mediant == UTT Neg 4 3 


{-
instance Invert UTT where
  invert (UTT Pos m n) = UTT Pos (invert m) (invert n)
  invert (UTT Neg m n) = UTT Neg (invert n) (invert m)  -- swapped (?)
-}


demo1 = invert (UTT Neg 4 5)
demo2 = invert (UTT Neg 8 9)

moveMode :: Mode -> Triad -> Triad 
moveMode Neg (Triad r Pos) = Triad r Neg
moveMode Neg (Triad r Neg) = Triad r Pos
moveMode _   t             = t

demo3 = (Triad 0 Pos) `act` (UTT Pos 4 3)
demo4 = (Triad 0 Pos) `act` (UTT Neg 4 3)
demo5 = (Triad 0 Neg) `act` (UTT Pos 4 3)

comp :: UTT -> UTT -> UTT
comp (UTT Pos a b) (UTT Pos c d) = UTT Pos (a+c) (b+d)
comp (UTT Neg a b) (UTT Neg c d) = UTT Pos (a+d) (b+c)
comp (UTT _   a b) (UTT _   c d) = UTT Neg (a+c) (b+d)