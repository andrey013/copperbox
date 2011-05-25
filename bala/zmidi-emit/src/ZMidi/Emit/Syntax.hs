{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit.Syntax
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module re-exports types and functions from 
-- "ZMidi.Emit.SyntaxInternal" but makes them opaque.
--
--------------------------------------------------------------------------------

module ZMidi.Emit.Syntax
  ( 

  -- * Type synonyms
    MidiPitch
  , MidiDuration
  , GMInst
  , GMDrum

  -- * Higher level syntax
  , HiMidi
  , Track
  , Voice
  , Section
  , Overlay
  , Primitive
  , VoiceMsg
  , PrimProps


  ) where

import ZMidi.Emit.SyntaxInternal

