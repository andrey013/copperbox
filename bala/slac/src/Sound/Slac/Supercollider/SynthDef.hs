{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.Slac.Supercolider.SynthDef
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC.
--
-- Shim module for SynthDefs
--
--------------------------------------------------------------------------------


module Sound.Slac.Supercollider.SynthDef
  (

  -- * Export SynthDef modules
    module Sound.Slac.Supercollider.SynthDef.Datatypes
  , module Sound.Slac.Supercollider.SynthDef.Pretty
  , module Sound.Slac.Supercollider.SynthDef.ReadFile
  , module Sound.Slac.Supercollider.SynthDef.WriteFile


  ) where


import Sound.Slac.Supercollider.SynthDef.Datatypes
import Sound.Slac.Supercollider.SynthDef.Pretty
import Sound.Slac.Supercollider.SynthDef.ReadFile
import Sound.Slac.Supercollider.SynthDef.WriteFile
