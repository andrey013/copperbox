{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.Datatypes
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Datatypes
--
--------------------------------------------------------------------------------

module Sound.FMSS.Datatypes
  (

    FMSynth(..)
  , ModulatorOsc(..)
  , CarrierOsc(..)

  , OscilType(..)
  , OscilFreq(..)
  , Link(..) 

  ) where


import Sound.FMSS.Utils.FormatCombinators

data FMSynth = FMSynth
      { fm_instr_num    :: Int
      , fm_sinetbl      :: Int
      , fm_amp_env      :: Maybe (String,Doc)
      , fm_mods         :: [ModulatorOsc]
      , fm_cars         :: [CarrierOsc]
      , fm_links        :: [Link]
      }
  deriving (Eq,Show)

type NodeId = Int

data ModulatorOsc = ModulatorOsc
      { mosc_num    :: NodeId
      , mosc_freq   :: OscilFreq 
      }
  deriving (Eq,Ord,Show)

data OscilType = CARRIER | MODULATOR
  deriving (Eq,Ord,Show)

data CarrierOsc = CarrierOsc 
      { cosc_num    :: NodeId
      , cosc_freq   :: OscilFreq
      }
  deriving (Eq,Ord,Show)


-- Note - looks like Modulators have either a modulation envelope, 
-- a reference to a common envelope or no envelope (no dymanic 
-- spectra change).
-- 
-- Carrier and modulator oscillators are probably different 
-- objects.
--

data OscilFreq = FixedFreq Double
               | BaseScaler Double
  deriving (Eq,Ord,Show)

-- | Note cycles can be Car-Car e.g. DX7 algorithm 32
--
data Link = ModMod  NodeId NodeId
          | ModCar  NodeId NodeId
          | Cycle   NodeId NodeId
  deriving (Eq,Ord,Show)
