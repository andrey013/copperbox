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
  , module ZMidi.Emit.VersionNumber


  -- * Type synonyms
  , MidiPitch
  , MidiDuration
  , GMInst
  , GMDrum

  -- * Data types
  , HiMidi
  , Track
  , Voice
  , Section
  , Overlay
  , Primitive
  , VoiceMsg
  , PrimProps

  -- * Output
  , writeHiMidi  -- re-export from Emit.OutputMidi

 
  ) where

import ZMidi.Emit.Construction
import ZMidi.Emit.Datatypes
import ZMidi.Emit.OutputMidi
import ZMidi.Emit.VersionNumber
