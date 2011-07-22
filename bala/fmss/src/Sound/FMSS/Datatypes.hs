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
  , EnvelopeSpec(..)
  , ModulatorOsc(..)
  , CarrierOsc(..)

  , OscilFreq(..)
  , Link(..) 

  ) where


import Sound.FMSS.AbstractSyntax
import Sound.FMSS.Utils.FormatCombinators

data FMSynth = FMSynth
      { fm_instr_num    :: Int
      , fm_sinetbl      :: Int
      , fm_envelopes    :: [EnvelopeSpec]
      , fm_mods         :: [ModulatorOsc]
      , fm_cars         :: [CarrierOsc]
      , fm_links        :: [Link]
      , fm_out          :: Stmt
      }

data EnvelopeSpec = EnvelopeSpec 
      { env_varname     :: VarId 
      , env_desc        :: Maybe String
      , env_opcode      :: String
      , env_expr_doc    :: Doc
      }

type NodeId = Int

data ModulatorOsc = ModulatorOsc
      { mosc_num        :: NodeId
      , mosc_freq       :: OscilFreq 
      , mosc_postpro    :: Maybe (Expr -> Expr)
      }


data CarrierOsc = CarrierOsc 
      { cosc_num        :: NodeId
      , cosc_freq       :: OscilFreq
      , cosc_postpro    :: Maybe (Expr -> Expr)
      }
 


-- Note - looks like Modulators have either a modulation envelope, 
-- a reference to a common envelope or no envelope (no dynamic 
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
