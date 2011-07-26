{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core
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

module Majalan.Core
  (

    module Majalan.Core.CsoundScore
  , module Majalan.Core.GenRoutines
  , module Majalan.Core.OutputCsound

  ) where


import Majalan.Core.CsoundScore
import Majalan.Core.GenRoutines
import Majalan.Core.OutputCsound


