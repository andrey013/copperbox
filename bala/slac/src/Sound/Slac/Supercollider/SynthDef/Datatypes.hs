{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.Slac.Supercolider.SynthDef.Datatypes
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC.
--
-- Datatypes to represent @synthdef@ files
--
--------------------------------------------------------------------------------


module Sound.Slac.Supercollider.SynthDef.Datatypes
  (

  -- * SynthDef file representation.
    SynthDefFile(..)
  , SynthDef(..)
  , ParamName(..)
  , UgenSpec(..)
  , InputSpec(..)
  , OutputSpec(..)

  ) where

import Data.Int

-- NOTE - must check if Int in the Supercollider docs means Int or Word.


data SynthDefFile = SynthDefFile 
      { synth_version   :: Int32             -- expected to be 1
      , synth_defs      :: [SynthDef] 
      }
  deriving (Eq,Ord,Show)

data SynthDef = SynthDef
      { synth_name      :: String
      , synth_consts    :: [Double]
      , synth_pvalues   :: [Double]
      , synth_pnames    :: [ParamName]
      , synth_ugens     :: [UgenSpec]     
      }
  deriving (Eq,Ord,Show)


data ParamName = ParamName
      { param_name      :: String
      , param_index     :: Int16
      }
  deriving (Eq,Ord,Show)

data UgenSpec = UgenSpec
      { ugen_name       :: String
      , ugen_calc_rate  :: Int8
      , ugen_kinput     :: Int16
      , ugen_koutput    :: Int16
      , ugen_sindex     :: Int16
      , ugen_inputs     :: [InputSpec]
      , ugen_outputs    :: [OutputSpec]
      }
  deriving (Eq,Ord,Show)


data InputSpec = InputKonst Int16
               | InputUgen  Int16
  deriving (Eq,Ord,Show)


data OutputSpec = OutputSpec
      { output_rate     :: Int8
      }
  deriving (Eq,Ord,Show)
