
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.TemplateDatatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Datatypes for elements extracted from score files.
--
--------------------------------------------------------------------------------

module HNotate.TemplateDatatypes where

import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.Pitch

import qualified Data.Foldable as F
import Data.Ratio
import Data.Sequence hiding (empty, length) 
import Text.PrettyPrint.Leijen (Doc)


-- A non-opaque SrcPos type  
data SrcPos = SrcPos { 
    _src_line     :: Int,
    _src_column   :: Int,
    _src_file     :: String
  }
  deriving (Eq,Ord,Show) 
  

data OutputScheme = OutputRelative | OutputDefault
  deriving (Eq,Show)
  
  
  

-- 1. Text view - (Water,Hole)
-- Preserves source text - the holes are collected at their 
-- source position for filling. 

type TextChunk = (String, Maybe SrcPos)


-- 2. 'Island Grammars' for Abc and LilyPond.
-- Only the elements that effect evaluation are retained. 

-- Common to both - meta output
-- e.g __ %# output: tune1 __
data MetaOutput  = MetaOutput (Maybe OutputScheme) String
  deriving (Eq,Show)

-- Common to both - directives that have no direct representation
-- in Abc or LilyPond.
-- e.g __ %# meter_pattern: 2+2+2/8 __ 
data MetaBinding = MetaMeterPattern MeterPattern
  deriving (Eq,Show)
  
   
-- 2a. A miniaturized Abc source file 
newtype AbcScore = AbcScore { getAbcScore :: [AbcTune] }
  deriving (Eq,Show)
  
-- X field gives the Int
data AbcTune = AbcTune Int [AbcExpr]
  deriving (Eq,Show)
  
data AbcExpr = AbcFieldBinding AbcField
             | AbcMetaBinding MetaBinding   -- shared with LilyPond
             | AbcOutput MetaOutput
  deriving (Eq,Show)             
             
data AbcField = AbcKey Key
              | AbcMeter Meter
  deriving (Eq,Show)

-- 2b. A miniaturized LilyPond source file    

data LyScore = LyScore { getLyScore :: [LyExpr]}
  deriving (Eq,Show) 
  
data LyExpr = LyCmdBinding LyCommand
            | LyMetaBinding MetaBinding   -- shared with LilyPond
            | LyOutput MetaOutput
            | LyNestExpr [LyExpr]
  deriving (Eq,Show)  

data LyCommand = LyCadenza Bool
               | LyKey Key
               | LyPartial Duration
               | LyRelative Pitch
               | LyTime Meter
  deriving (Eq,Show)
  
-- 3. Expression view
-- Expressions of interest (e.g. time signatures, key signatures)
-- and meta comments, are extracted from the original file (via a 
-- preprocessing step). Everything else is dropped. 
--
-- The syntax is pattern atfer the lambda calculus so that it is 
-- trivial to evaluate. 



 
data Expr = Let Binding Expr
          | SDo OutputDirective Expr
          | Do OutputDirective
          | Fork Expr Expr
  deriving (Eq,Show)   
  
            

data Binding = LetCadenza Bool
             | LetKey Key
             | LetMeter Meter
             | LetMeterPattern MeterPattern
             | LetPartial Duration
             | LetRelativePitch Pitch
             | LetNone                -- Used for 'X' fields in Abc
  deriving (Eq,Show)             

type NoteListName = String

data OutputDirective = OutputDirective (Maybe OutputScheme) NoteListName  
  deriving (Eq,Show)
  
newtype Hoas = Hoas { getExprs :: [HoasExpr] }


data HoasExpr = HLet (Env -> Env) HoasExpr
              | HSDo OutputDirective HoasExpr
              | HDo OutputDirective
              | HFork HoasExpr HoasExpr
              

toHoas :: [Expr] -> Hoas
toHoas xs = Hoas $ map convExpr xs

convExpr :: Expr -> HoasExpr
convExpr (Let b e)    = HLet (convBinding b) (convExpr e)
convExpr (SDo o e)    = HSDo o (convExpr e)
convExpr (Do o)       = HDo o
convExpr (Fork e e')  = HFork (convExpr e) (convExpr e')

convBinding :: Binding -> (Env -> Env)
convBinding (LetCadenza b)        = set_cadenza b
convBinding (LetKey k)            = set_current_key k
convBinding (LetMeter m)          = set_current_meter m
convBinding (LetMeterPattern mp)  = set_meter_pattern mp
convBinding (LetPartial d)        = set_partial_measure d
convBinding (LetRelativePitch p)  = set_relative_pitch p
convBinding (LetNone)             = id

transMetaOutput :: MetaOutput -> OutputDirective 
transMetaOutput (MetaOutput oscm name) = OutputDirective oscm name
 
transMetaBinding :: MetaBinding -> Binding 
transMetaBinding (MetaMeterPattern mp) = LetMeterPattern mp
                



  
