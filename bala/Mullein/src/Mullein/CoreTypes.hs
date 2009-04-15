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


--------------------------------------------------------------------------------
-- Note lists

type NoteList = [Element]


--------------------------------------------------------------------------------
-- Musical representation


type MeterPattern = [Duration] 

type MetricalSpec = (Meter,MeterPattern)

  
data Key = Key PitchLabel Mode [PitchLabel]
  deriving (Eq,Show) 

  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving (Eq,Enum,Ord,Show) 


newtype LabelSet = LabelSet { getLabelSet :: Map.Map Int PitchLabel }
  deriving (Show)
           