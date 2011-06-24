{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}


module SimpleClick where


import ZSnd.Core.Inst.Click
import ZSnd.Core.Inst.Prim
import ZSnd.Core.Utils.FormatCombinators


-- Note - Click declares Out, so we do too (even though we have 
-- special syntax for it as well).
--


-- figure 1.23 CSB

instr115 :: CExpr
instr115 = LetD k1 linenX 
         ( LetD k2 exponX 
         ( LetD a1 buzzX
         ( LetD o1 outX
         ( LetC (k1,0) (o1,0)
         ( LetC (k2,0) (a1,0)
         ( LetC (a1,0) (o1,1)
                (Out o1) ))))))
               
  where
    k1 = 1 
    k2 = 2
    a1 = 3
    o1 = 4

linenX :: Element
linenX = Element "linen" [CPfield 4, CPfield 7, CPfield 3, CPfield 8]
                         (Out1 K)

exponX :: Element
exponX = Element "expon" [CPfield 9, CPfield 3, CPfield 10]
                         (Out1 K)

buzzX :: Element
buzzX = Element "buzz" [DLiteral 1, CPfield 5, ClkPort 0, CPfield 6 ]
                       (Out1 A)


outX :: Element 
outX = Element "outs" [ (1 + ClkPort 0) * ClkPort 1 ]
                      Out0


demo01 = format instr115


demo02 = either text format $ translateDesc 115 instr115


{-

oscil_1_3 :: Element
oscil_1_3 = Element "oscil" [DLiteral 10000, DLiteral 440, ILiteral 1] 
                            [LocVar A "1"]

demo01 = format oscil_1_3

-}