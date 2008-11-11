
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

import HNotate.Document
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.Pitch

import qualified Data.Foldable as F
import Data.Ratio
import Data.Sequence hiding (empty, length) 



-- SrcLoc - SourcePos without the file name 
data SrcLoc = SrcLoc { 
    _src_line     :: Int,
    _src_column   :: Int
  }
  deriving (Eq,Ord,Show) 
  

data OutputScheme = OutputRelative | OutputDefault
  deriving (Eq,Show)
  
  
  

-- 1. Text view - (Water,Hole)
-- Preserves source text - the holes are collected at their 
-- source position for filling. 
type Water = String

data TextSource = SourceFile Water Source'
  deriving (Show)
  
data Source' = EndOfSource | Island SrcLoc TextSource  
  deriving (Show)

-- 2. 'Island Grammars' for Abc and LilyPond.
-- Only the elements that effect evaluation are retained. 

-- Common to both - meta output
-- e.g __ %# output: tune1 __
data MetaOutput  = MetaOutput (Maybe OutputScheme) String
  deriving (Eq,Show)

-- Directives specified in meta-comments.
-- Some are common to both Abc or LilyPond - directives that have 
-- no direct representation, e.g __ %# meter_pattern: 2+2+2/8 __
-- Others are representable in one format but not the other, 
-- e.g. anacrusis (\partial) is representable in LilyPond but not
-- Abc.
data MetaBinding = MetaMeterPattern MeterPattern
                 | MetaPartial Duration
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

-- Note a keyfield isn't guaranteed to have a key signature
-- it might have a clef designation instead.              
data AbcField = AbcKey (Maybe Key) 
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
  
newtype ParaHoas a = Hoas { getExprs :: [HoasExpr a] }


data HoasExpr a = HLet (Env -> Env) a (HoasExpr a)
                | HSDo OutputDirective (HoasExpr a)
                | HDo OutputDirective
                | HFork (HoasExpr a) (HoasExpr a)
                | HText a (HoasExpr a)
              
type Hoas       = ParaHoas ()
type HoasExprU  = HoasExpr () 

type DocuHoas   = ParaHoas (ODoc -> ODoc)
type HoasExprD  = HoasExpr (ODoc -> ODoc)


toHoas :: [Expr] -> Hoas
toHoas xs = Hoas $ map convExpr xs

-- nothing creates HText, it is only created in the DocuHoas interpretation 
convExpr :: Expr -> HoasExpr ()
convExpr (Let b e)    = HLet (convBinding b) () (convExpr e)
convExpr (SDo o e)      = HSDo o (convExpr e)
convExpr (Do o)         = HDo o
convExpr (Fork e e')    = HFork (convExpr e) (convExpr e')


convBinding :: Binding -> (Env -> Env)
convBinding (LetCadenza b)        = set_unmetered b
convBinding (LetKey k)            = set_current_key k
convBinding (LetMeter m)          = set_current_meter m
convBinding (LetMeterPattern mp)  = set_meter_pattern mp
convBinding (LetPartial d)        = set_anacrusis d
convBinding (LetRelativePitch p)  = set_relative_pitch p
convBinding (LetNone)             = id

transMetaOutput :: MetaOutput -> OutputDirective 
transMetaOutput (MetaOutput oscm name) = OutputDirective oscm name
 
transMetaBinding :: MetaBinding -> Binding 
transMetaBinding (MetaMeterPattern mp)  = LetMeterPattern mp
transMetaBinding (MetaPartial d)        = LetPartial d
                



  
