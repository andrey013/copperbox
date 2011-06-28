{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}



module NewPortSpec where

import ZSnd.Core.CsoundInst.Click
import ZSnd.Core.CsoundInst.Index
import ZSnd.Core.CsoundInst.Monadic hiding ( out )
import ZSnd.Core.CsoundInst.Typed

import ZSnd.Core.Utils.FormatCombinators hiding ( line )


demo01 = either print (print . format) $ translateDesc 0 instr0
demo02 = either print (print . format) $ translateDesc 115 instr115


instr0 :: [CStmt]
instr0 = 
    [ DeclE o1 (getElementUniv $ out $ port0_1 440.0)
    , Out o1
    ]
  where
    o1 = mkElemRef 0



instr115 :: [CStmt]
instr115 =  
    [ DeclE o1 (getElementA $ out $ port2_1 $ \kx1 ax1 -> ax1 * cast kx1 )
    , DeclE k1 (getElementK $ linen $ port0_4 (pfield 4, pfield 7, pfield 3, pfield 8))
    , DeclE k2 (getElementK $ expon $ port0_3 (pfield 9, pfield 3, pfield 10))
    , DeclE a1 (getElementA $ buzz  $ port1_4 $ \kx2 -> (1, pfield 5, kx2 + 1, pfield 6))
    , Conn (k1,0) (o1,0)
    , Conn (k2,0) (a1,0)
    , Conn (a1,0) (o1,1)
    , Out o1
    ]          
  where
    k1 = mkElemRef 1 
    k2 = mkElemRef 2
    a1 = mkElemRef 3
    o1 = mkElemRef 4


out :: Opcode1 ARate -> Element ARate
out opF =  mkElement "out" inspec Out0
  where
    inspec = applyOpcode opF $ \a -> [ getConfA a ]



linen :: forall rate1 rate. (KA_Rate rate) 
       => Opcode4 rate1 IRate IRate IRate -> Element rate
linen opF = 
    mkElement "linen" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(a,b,c,d) -> [ getConfUniv a
                                             , getConfI b
                                             , getConfI c
                                             , getConfI d ]


expon :: forall rate. (KA_Rate rate) 
       => Opcode3 IRate IRate IRate -> Element rate
expon opF = 
    mkElement "expon" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(a,b,c) -> [ getConfI a
                                           , getConfI b
                                           , getConfI c ] 


buzz :: Opcode4 rate1 rate2 KRate IRate -> Element ARate
buzz opF = 
    mkElement "buzz" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(a,b,c,d) -> [ getConfUniv a
                                             , getConfUniv b
                                             , getConfK c
                                             , getConfI d ]


line :: Opcode3 IRate IRate IRate -> UElement
line opF =  UElement "line" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(a,b,c) -> [ getConfI a
                                           , getConfI b
                                           , getConfI c]



dummy1 :: Conf IRate -> Conf KRate -> Config3 IRate IRate IRate
dummy1 = \a b -> (a * 440, pfield 4, cast b)



dummy2 :: UElement
dummy2 = line $ port2_3 dummy1

dummy3 :: UElement
dummy3 = line $ port0_3 (440, pfield 1, pfield 3)





-- Can specify @line@ as 3 /Arg ports/ - promoting them to a 
-- decl-level port of 3?
--


-- Statically we know opcode name (\"line\") and output arity is 1.
-- 
-- We want a Port description that generates three values...
--
