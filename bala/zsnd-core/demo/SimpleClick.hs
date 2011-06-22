{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}


module SimpleClick where


import ZSnd.Core.Inst.Click
import ZSnd.Core.Inst.Prim
import ZSnd.Core.Utils.FormatCombinators


-- Note - Click declares Out, so we do too

-- Ports in the Click lang do not necessarily correspond to
-- Ports in the Prim lang...

-- figure 1.23 CSB

instr115 :: CExpr
instr115 = LetE k1 linenX 
         ( LetE k2 exponX 
         ( LetE a1 buzzX
         ( LetE o1 outX
         ( Sequ [ (k1,0) :-> (o1,0)
                , (k2,0) :-> (a1,0)
                , (a1,0) :-> (o1,1)
                , Out o1
                ]))))
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
buzzX = Element "buzz" [DLiteral 1, CPfield 5, CVar 1, CPfield 6 ]
                       (Out1 A)


outX :: Element 
outX = Element "out" [ CVar 1 .*.  CVar 1 ]
                     Out0


demo01 = format instr115

(.*.) :: InConf -> InConf -> InConf
(.*.) = CBinOp "*"


demo02 = either text format $ translateDesc 115 instr115


{-

oscil_1_3 :: Element
oscil_1_3 = Element "oscil" [DLiteral 10000, DLiteral 440, ILiteral 1] 
                            [LocVar A "1"]

demo01 = format oscil_1_3

-}