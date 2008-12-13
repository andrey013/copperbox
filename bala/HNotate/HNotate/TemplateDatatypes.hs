{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

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
import qualified HNotate.FPList as Fpl 
import HNotate.MusicRepDatatypes
import HNotate.Pitch
import HNotate.ProcessingBase


-- SrcLoc - SourcePos without the file name 
data SrcLoc = SrcLoc { 
    _src_line     :: Int,
    _src_column   :: Int
  }
  deriving (Eq,Ord,Show) 
              


-- 1. Text view - (Water,Hole)
-- Preserves source text - the holes are collected at their 
-- source position for filling. 
type Water = String

type ParsedTemplate   = Fpl.FPList Water SrcLoc


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
          | DoExpr OutputDirective Expr
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
  
newtype Hoas = Hoas { getHoasExprs :: [HoasExpr] }


data HoasExpr = HLet (Env -> Env) HoasExpr
              | HDoExpr OutputDirective HoasExpr
              | HDo OutputDirective
              | HFork HoasExpr HoasExpr

-- For building Hoas with documents...

type HandBuiltTemplate = Fpl.FPList ODocS ()

data HandBuiltLilyPond = HBLP { 
      getHBLP :: (HandBuiltTemplate, [Maybe HoasExpr])
   }

data HandBuiltAbc = HBAbc { 
      getHBAbc :: (HandBuiltTemplate, [Maybe HoasExpr])
   }

type BuildDocS = (ODocS, Maybe HoasExpr) -> (ODocS, Maybe HoasExpr)

type OptHoasExprS  = Maybe HoasExpr -> Maybe HoasExpr

-- Note, we always bookend the PluggableBuildDoc with (<> emptyDoc)
-- this is to get a proper FPList (e.g. @abababa@ rather than @ababab@)    
buildDocsContents :: [BuildDocS] -> (HandBuiltTemplate, [Maybe HoasExpr])
buildDocsContents = combine . unzip . map content where
    combine (odocs,oes) = (makePDoc odocs, oes)
    -- see note above
    makePDoc :: [ODocS] -> HandBuiltTemplate 
    makePDoc []     = Fpl.singleton (<> emptyDoc)   
    makePDoc (w:ws) = Fpl.dblcons w () rest where rest = makePDoc ws


content :: BuildDocS -> (ODocS, Maybe HoasExpr)
content f = f ((<> emptyDoc), Nothing)

zero_doc :: (ODoc , Maybe HoasExpr)
zero_doc = (emptyDoc, Nothing)



ohlet ::  (Env -> Env) -> OptHoasExprS
ohlet _   Nothing     = Nothing
ohlet upd (Just e)    = Just $ HLet upd e

ohdo ::  OutputDirective -> OptHoasExprS
ohdo d Nothing     = Just $ HDo d
ohdo d (Just e)    = Just $ HDoExpr d e


buildDocHoas :: (ODocS,OptHoasExprS) -> BuildDocS
buildDocHoas (docS,oeS) (docS',oe) = (docS . docS', oeS oe)

buildDocOnly :: ODocS -> BuildDocS 
buildDocOnly docS (docS',oe) = (docS . docS',oe)

buildExprOnly :: (Maybe HoasExpr -> Maybe HoasExpr) -> BuildDocS 
buildExprOnly exprS (docS,oe) = (docS,exprS oe)


toHoas :: [Expr] -> Hoas
toHoas xs = Hoas $ map convExpr xs


convExpr :: Expr -> HoasExpr
convExpr (Let b e)      = HLet (convBinding b) (convExpr e)
convExpr (DoExpr o e)   = HDoExpr o (convExpr e)
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
                



  
