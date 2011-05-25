{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Top level /shim/ for the Emit modules.
-- 
-- Projects should only import this module. 
-- 
-- Note - all the syntax datatypes are opaque. Syntax is built 
-- with functions in the @ZMidi.Emit.Construction@ module.
--
--------------------------------------------------------------------------------

module ZMidi.Emit
  ( 

    module ZMidi.Emit.Construction
  , module ZMidi.Emit.GMDrums
  , module ZMidi.Emit.GMInstruments
  , module ZMidi.Emit.OutputMidi
  , module ZMidi.Emit.Syntax
  , module ZMidi.Emit.VersionNumber



 
  ) where

import ZMidi.Emit.Construction
import ZMidi.Emit.GMDrums
import ZMidi.Emit.GMInstruments
import ZMidi.Emit.OutputMidi
import ZMidi.Emit.Syntax
import ZMidi.Emit.VersionNumber
