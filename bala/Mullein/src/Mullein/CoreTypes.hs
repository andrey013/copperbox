{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.CoreTypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Fundamental datatypes
--
--------------------------------------------------------------------------------

module Mullein.CoreTypes where

import Mullein.Duration
import Mullein.Gen.Syntax ( Element(..) )
import Mullein.Pitch

import qualified Data.Map as Map
import qualified Data.Sequence as S



--------------------------------------------------------------------------------
-- Note lists

type NoteList = S.Seq Element


--------------------------------------------------------------------------------
-- 

type Part env = S.Seq (Z env)
type Motive env = (env, NoteList)

data Z env = Repeated (Motive env)
           | FSRepeat (Motive env) (Motive env) (Motive env)
           | Literal  (Motive env)
 deriving (Show)


--------------------------------------------------------------------------------
-- Musical representation

-- For /universality/ meter is defined according to Abc's representation.
-- LilyPond will simply generate @TimeSig@ cases.
data Meter = TimeSig Integer Integer 
           -- | CommonTime is 4/4
           | CommonTime 
           -- | CutTime is 2/2
           | CutTime
  deriving (Eq,Show)

type MeterPattern = [Duration] 

type MetricalSpec = (Meter,MeterPattern)

  
data Key = Key PitchLabel Mode [PitchLabel]
  deriving (Eq,Show) 

  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving (Eq,Enum,Ord,Show) 


newtype LabelSet = LabelSet { getLabelSet :: Map.Map Int PitchLabel }
  deriving (Show)
           