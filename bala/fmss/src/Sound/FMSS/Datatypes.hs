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
  , SynthBody(..)
  , EnvelopeSpec(..)
  , Modulator(..)
  , Carrier(..)
  , Oscil(..)

  , OscilInputZero(..)
  , Link(..) 
  , Cycle(..)

  , Oscillator(..)

  ) where


import Sound.FMSS.AbstractSyntax

data FMSynth = FMSynth
      { fm_instr_num    :: Int
      , fm_sinetbl      :: Int
      , fm_decls        :: [Decl]
      , fm_synth_body   :: SynthBody
      , fm_out          :: Stmt
      }

data SynthBody = SynthBody 
      { synth_mods      :: [Modulator]
      , synth_cars      :: [Carrier]
      , synth_links     :: [Link]
      , synth_cycles    :: [Cycle]
      } 


data EnvelopeSpec = EnvelopeSpec 
      { env_varname     :: VarId 
      , env_desc        :: Maybe String
      , env_opcode      :: String
      , env_exprs       :: [SymDouble]
      }

type NodeId = Int

data Modulator = Modulator 
      { mod_num     :: Int
      , mod_oscil   :: Oscil 
      }


data Carrier = Carrier 
      { car_num     :: Int 
      , car_oscil   :: Oscil 
      } 


-- | Number should potentially be a property of Carrier or 
-- Modulator, as number might be assigned as a uid in a monad...
--
data Oscil = Oscil 
      { oscil_input_zero    :: OscilInputZero
      , oscil_postpro       :: Maybe (Expr -> Expr)
      } 


class Oscillator osc where
  oscilNum          :: osc -> NodeId
  oscilInputZero    :: osc -> OscilInputZero
  oscilPostpro      :: osc -> Maybe (Expr -> Expr)


instance Oscillator Modulator where
  oscilNum          = mod_num
  oscilInputZero    = oscil_input_zero . mod_oscil
  oscilPostpro      = oscil_postpro . mod_oscil


instance Oscillator Carrier where
  oscilNum          = car_num
  oscilInputZero    = oscil_input_zero . car_oscil
  oscilPostpro      = oscil_postpro . car_oscil


-- Note - looks like Modulators have either a modulation envelope, 
-- a reference to a common envelope or no envelope (no dynamic 
-- spectra change).
-- 
-- Carrier and modulator oscillators are probably different 
-- objects.
--

data OscilInputZero = FixedInput Expr
                    | BaseFreqScaler ExprF

  -- Potentially oscilFreq could be extended to OscilInput 
  -- tosupport noide inputs.


data Link = ModMod  NodeId NodeId
          | ModCar  NodeId NodeId
  deriving (Eq,Ord,Show)


-- | Note cycles can be Car-Car e.g. DX7 algorithm 32
--
-- The @ExprF @ is used in the assignment to the @aphm@ variable.
--
data Cycle = CycModMod   NodeId NodeId ExprF
           | CycCarCar   NodeId NodeId ExprF
           | CycCarMod   NodeId NodeId ExprF
