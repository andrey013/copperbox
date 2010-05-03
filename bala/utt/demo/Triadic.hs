

module Triadic where

import UTT.Base
import UTT.Neume
import UTT.TCBase

test01 :: Bool
test01 = invert dominant == UTT Pos 7 7

test02 :: Bool 
test02 = invert mediant == UTT Neg 4 3 



demo1 = invert (UTT Neg 4 5)
demo2 = invert (UTT Neg 8 9)


demo3 = (Triad 0 Pos) `act` (UTT Pos 4 3)
demo4 = (Triad 0 Pos) `act` (UTT Neg 4 3)
demo5 = (Triad 0 Neg) `act` (UTT Pos 4 3)

