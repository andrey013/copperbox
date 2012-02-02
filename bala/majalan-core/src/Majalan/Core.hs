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

    module Majalan.Core.GenRoutines
  , module Majalan.Core.OutputCsound
  , module Majalan.Core.Score

  ) where


import Majalan.Core.GenRoutines
import Majalan.Core.OutputCsound
import Majalan.Core.Score


