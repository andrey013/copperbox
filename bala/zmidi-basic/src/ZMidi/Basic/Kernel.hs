{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Top level /shim/ for the Kernel modules.
-- 
-- Projects should only import this module. 
-- 
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel
  ( 


    module ZMidi.Basic.Kernel.Base.OutputMidi
  , module ZMidi.Basic.Kernel.Base.Syntax
  , module ZMidi.Basic.Kernel.Objects.GMDrums
  , module ZMidi.Basic.Kernel.Objects.GMInstruments


 
  ) where

import ZMidi.Basic.Kernel.Base.Syntax
import ZMidi.Basic.Kernel.Base.OutputMidi
import ZMidi.Basic.Kernel.Objects.GMDrums
import ZMidi.Basic.Kernel.Objects.GMInstruments
