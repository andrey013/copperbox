{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes.Assignment
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Psuedo opcodes for \"assigment\". 
-- 
--------------------------------------------------------------------------------

module ZSnd.Core.Opcodes.Assignment
  (

   -- * Addition
    add2
  , adds

   -- * Addition
  , sub2
  , subs

  -- * Multiplication
  , mult2
  , mults

  -- * division
  , div2


  , opexp
  , opabs

  ) where


import ZSnd.Core.CsoundInst.Click
import ZSnd.Core.CsoundInst.Index
import ZSnd.Core.CsoundInst.Prim
import ZSnd.Core.CsoundInst.Typed



-- Addition

add2 :: forall rate. (Rate rate) 
     => Opcode2 rate rate -> Element rate
add2 opF = 
    mkInfixAssign (infixL 6 "+") inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(x1,x2) -> 
                [ getConfUniv x1, getConfUniv x2 ]

adds :: forall rate. (Rate rate) 
     => OpcodeList1 rate -> Element rate
adds opF = 
    mkInfixAssign (infixL 6 "+") inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \xs -> 
                map getConfUniv xs


-- Subtraction

sub2 :: forall rate. (Rate rate) 
     => Opcode2 rate rate -> Element rate
sub2 opF = 
    mkInfixAssign (infixL 6 "-") inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(x1,x2) -> 
                [ getConfUniv x1, getConfUniv x2 ]

subs :: forall rate. (Rate rate) 
     => OpcodeList1 rate -> Element rate
subs opF = 
    mkInfixAssign (infixL 6 "-") inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \xs -> 
                map getConfUniv xs


-- Multiplication

mult2 :: forall rate. (Rate rate) 
      => Opcode2 rate rate -> Element rate
mult2 opF = 
    mkInfixAssign (infixL 7 "*") inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(x1,x2) -> 
                [ getConfUniv x1, getConfUniv x2 ]

mults :: forall rate. (Rate rate) 
      => OpcodeList1 rate -> Element rate
mults opF = 
    mkInfixAssign (infixL 7 "*") inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \xs -> 
                map getConfUniv xs


-- Division

div2 :: forall rate. (Rate rate) 
     => Opcode2 rate rate -> Element rate
div2 opF = 
    mkInfixAssign (infixL 7 "/") inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(x1,x2) -> 
                [ getConfUniv x1, getConfUniv x2 ]


opexp :: forall rate. (Rate rate) 
     => Opcode1 rate -> Element rate
opexp opF = 
    mkPrefixAssign "exp" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \x1 -> 
                [ getConfUniv x1 ]

opabs :: forall rate. (Rate rate) 
     => Opcode1 rate -> Element rate
opabs opF = 
    mkPrefixAssign "abs" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \x1 -> 
                [ getConfUniv x1 ]

