{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZScore
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
-- @ZScore.Opcodes@ must be imported explicitly.
--

--------------------------------------------------------------------------------

module ZScore
  (
    module ZScore.GenRoutines
  , module ZScore.CsoundInst
  , module ZScore.CsoundScore
  , module ZScore.OutputCsound

  ) where


import ZScore.GenRoutines
import ZScore.CsoundInst
import ZScore.CsoundScore
import ZScore.OutputCsound



