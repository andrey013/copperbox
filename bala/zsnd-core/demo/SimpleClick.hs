{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}


module SimpleClick where


import ZSnd.Core.CsoundInst
import ZSnd.Core.Inst.Click
import ZSnd.Core.Opcodes
import ZSnd.Core.Utils.FormatCombinators


-- Note - the final stmt Out should force the @out@ opcode 
-- invocation to be printed last. It does not actually invoke
-- the printing of @out@ - all declared invocations will be
-- printed provided they are saturated.
-- 
-- If there are unsaturated invocations an error is thrown.
-- 

-- figure 1.23 CSB

instr115 :: [CStmt]
instr115 =  
    [ DeclE o1 (getElementA outX)
    , DeclE k1 (getElementK linenX)
    , DeclE k2 (getElementK exponX)
    , DeclE a1 (getElementA buzzX)
    , DeclV a2 (UBinding (CPfield 4) A)
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
    a2 = mkElemRef 5


type ISig = Element IRate
type KSig = Element KRate
type ASig = Element ARate



linenX :: forall rate. (KA_Rate rate) => Element rate
linenX = mkElement "linen" [CPfield 4, CPfield 7, CPfield 3, CPfield 8]
                           (Out1 $ dataRate (undefined :: rate))

exponX :: forall rate. (KA_Rate rate) => Element rate
exponX = mkElement "expon" [CPfield 9, CPfield 3, CPfield 10]
                           (Out1 $ dataRate (undefined :: rate))
 
buzzX :: forall rate. (KA_Rate rate) => Element rate
buzzX = mkElement "buzz" [DLiteral 1, CPfield 5, ClkPort 0, CPfield 6 ]
                         (Out1 $ dataRate (undefined :: rate))


outX :: forall rate. (KA_Rate rate) => Element rate
outX = mkElement "outs" [ (1 + ClkPort 0) * ClkPort 1 ]
                        Out0


demo01 = vcat $ map format instr115


demo02 = either text format $ translateDesc 115 instr115


example1 :: Inst ()
example1 = do 
  k1 <- klet linenX
  k2 <- klet exponX
  a1 <- alet buzzX
  o1 <- alet outX
  k1     =>= o1
  k2     =>= a1
  a1     =>- (o1,1)
  out o1

demo03 = either text format $ runInst 115 example1

