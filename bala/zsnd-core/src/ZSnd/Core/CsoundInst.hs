{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CsoundInst
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

module ZSnd.Core.CsoundInst
  (

    module ZSnd.Core.CsoundInst.Click
  , module ZSnd.Core.CsoundInst.Index
  , module ZSnd.Core.CsoundInst.Monadic
  , module ZSnd.Core.CsoundInst.Typed

  ) where

import ZSnd.Core.CsoundInst.Click
import ZSnd.Core.CsoundInst.Index
import ZSnd.Core.CsoundInst.Monadic
import ZSnd.Core.CsoundInst.Typed
