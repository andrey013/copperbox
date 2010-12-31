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
  , module ZMidi.Emit.Constructors
  , module ZMidi.Emit.OutputMidi
  , module ZMidi.Emit.VersionNumber

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

  , hiMidi
  , addTrack
  , track
 
  ) where

import ZMidi.Emit.Builder
import ZMidi.Emit.Constructors
import ZMidi.Emit.Datatypes
import ZMidi.Emit.OutputMidi
import ZMidi.Emit.VersionNumber
