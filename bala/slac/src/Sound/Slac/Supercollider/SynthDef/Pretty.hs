{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.Slac.Supercolider.SynthDef.Pretty
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


module Sound.Slac.Supercollider.SynthDef.Pretty
  (

  -- * SynthDef file representation.
    ppSynthDef

  ) where


import Sound.Slac.Supercollider.SynthDef.Datatypes

import Text.PrettyPrint.Leijen                  -- package: wumpus-basic




-- NOTE - must check if Int in the Supercollider docs means Int or Word.


ppSynthDef :: SynthDef -> Doc
ppSynthDef s = lparen <$> body <$> rparen
  where
    body = text "SynthDef.new" <> parens (dquotes $ text $ synth_name s)


-- ppArgs 


{-

newtype SynthDefFile = SynthDefFile { synth_defs :: [SynthDef] }
  deriving (Eq,Ord,Show)

data SynthDef = SynthDef
      { synth_name      :: String
      , synth_consts    :: [Double]
      , synth_params    :: [ParamName]
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

-}