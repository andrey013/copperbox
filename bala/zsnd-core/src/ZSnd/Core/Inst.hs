{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Inst
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Shim module for the instrument code.
--
--------------------------------------------------------------------------------

module ZSnd.Core.Inst
  (
    module ZSnd.Core.Inst.Index
  , module ZSnd.Core.Inst.MonadicDefn
  ) where

import ZSnd.Core.Inst.Index
import ZSnd.Core.Inst.MonadicDefn
