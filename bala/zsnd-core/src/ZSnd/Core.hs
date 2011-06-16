{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Shim module
--
-- Note - to avoid Prelude name clashes the module 
-- @ZSnd.Core.Opcodes@ must be imported explicitly.
--

--------------------------------------------------------------------------------

module ZSnd.Core
  (
    module ZSnd.Core.GenRoutines
  , module ZSnd.Core.CsoundInst
  , module ZSnd.Core.CsoundScore
  , module ZSnd.Core.OutputCsound


  ) where


import ZSnd.Core.GenRoutines
import ZSnd.Core.CsoundInst
import ZSnd.Core.CsoundScore
import ZSnd.Core.OutputCsound



