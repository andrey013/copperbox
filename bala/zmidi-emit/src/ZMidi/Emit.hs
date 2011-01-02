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
--------------------------------------------------------------------------------

module ZMidi.Emit
  ( 

    module ZMidi.Emit.Builder
  , module ZMidi.Emit.Construction
  , module ZMidi.Emit.VersionNumber


  -- * Type synonyms
  , MidiPitch
  , MidiDuration
  , GMInst
  , GMDrum

  -- * Data types
  , HiMidi
  , Track
  , ChannelStream
  , Section
  , Overlays
  , SectionVoice
  , Primitive
  , VoiceMsg
  , PrimProps

  -- * Output
  , writeHiMidi  -- export from Emit.OutputMidi

 
  ) where

import ZMidi.Emit.Builder
import ZMidi.Emit.Construction
import ZMidi.Emit.Datatypes
import ZMidi.Emit.OutputMidi
import ZMidi.Emit.VersionNumber
