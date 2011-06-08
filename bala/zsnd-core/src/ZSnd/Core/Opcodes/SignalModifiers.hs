{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes.SignalModifiers
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Signal modifying opcodes.
-- 
--------------------------------------------------------------------------------

module ZSnd.Core.Opcodes.SignalModifiers
  (

  -- * Standard filters
    portk

  ) where



portk :: Expr KR -> Expr KR -> InstBuilder (Expr KR)
portk ksig khtim = opcode "portk" [ getExpr ksig, getExpr khtim ]



