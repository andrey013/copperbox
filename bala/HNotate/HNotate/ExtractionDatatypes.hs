
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ExtractionDatatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Datatypes for elements extracted from score templates.
--
--------------------------------------------------------------------------------

module HNotate.ExtractionDatatypes where

import HNotate.Duration
import HNotate.Pitch

import qualified Data.Foldable as F
import Data.Ratio
import Data.Sequence hiding (empty, length) 

type Idx = Int

-- Two views of a file

-- 1. Source preserving view 
-- Preserves source text - the holes are indexed for filling. 

newtype SPV = SPV { getTextElements :: Seq TextElement } 
  deriving (Show) 
  
data TextElement = SourceText String | MetaMark Idx SrcPos MetaDirective
  deriving (Show)

type Name = String
type Scheme = String
data MetaDirective = MetaOutput Name Scheme
  deriving Show
  
data SrcPos = SrcPos { 
    _src_line     :: Int,
    _src_column   :: Int,
    _src_file     :: String
  }
  deriving (Show)  

-- 2. Partially interpreted view
-- Expressions of interest (e.g. time signatures, key signatures) are
-- interpreted as are output directives inside meta comments, 
-- nesting structure is approximately respected. 
newtype PIV = PIV { getScoreElement :: [ScoreElement] }

data ScoreElement = Command Command
                  | Directive Idx MetaDirective
                  | Nested [ScoreElement]
  deriving Show                 
                 


data Command = CmdKey Key
             | CmdMeter Meter
             | CmdDefaultNoteLength Duration  -- Abc only
             | CmdRelativePitch Pitch         -- Lilypond only
             | CmdPartial Int Int
  deriving Show
 
                      
--------------------------------------------------------------------------------
-- Music representation

data Key = Key Pitch Mode
  deriving Show  

data Meter = TimeSig Int Int | CommonTime | CutTime
  deriving Show
  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving Show


c_major = Key middle_c Major  

four_four = TimeSig 4 4




  
