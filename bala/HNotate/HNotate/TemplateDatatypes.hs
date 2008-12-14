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

import HNotate.CommonUtils
import HNotate.Document
import HNotate.Duration
import HNotate.Env
import HNotate.FPList hiding (length)
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

type ParsedTemplate   = FPList Water SrcLoc


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

type HandBuiltTemplate = FPList ODocS ()

data LilyPondTemplate = LyTemplate { 
      getLyTemplate :: (HandBuiltTemplate, [Maybe HoasExpr])
   }

data AbcTemplate = AbcTemplate { 
      getAbcTemplate :: (HandBuiltTemplate, [Maybe HoasExpr])
   }


type BuildCombinedS a = (a -> a, Maybe HoasExpr) -> (a -> a, Maybe HoasExpr)
type BuildDocS = BuildCombinedS ODoc


type OptHoasExprS  = Maybe HoasExpr -> Maybe HoasExpr

-- Note, we always bookend the HandBuiltTemplate with (<> emptyDoc)
-- this is to get a proper FPList (e.g. @abababa@ rather than @ababab@)    
buildDocsContents :: [BuildDocS] -> (HandBuiltTemplate, [Maybe HoasExpr])
buildDocsContents = combine . unzip . map content where
    combine (odocs,oes) = (makePDoc odocs, oes)
    -- see note above
    makePDoc :: [ODocS] -> HandBuiltTemplate 
    makePDoc []     = singleton (<> emptyDoc)   
    makePDoc (w:ws) = dblcons w () rest where rest = makePDoc ws


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


buildCombinedS :: ((a -> a), OptHoasExprS) -> BuildCombinedS a
buildCombinedS (srcS,oeS) (srcS',oe) = (srcS . srcS', oeS oe)

buildSrcOnlyS :: (a -> a) -> BuildCombinedS a 
buildSrcOnlyS srcS (srcS',oe) = (srcS . srcS',oe)

buildExprOnlyS :: (Maybe HoasExpr -> Maybe HoasExpr) -> BuildCombinedS a 
buildExprOnlyS exprS (srcS,oe) = (srcS,exprS oe)


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
                

-- Plugging a parsed doc is nice and simple. 
-- Particularly, any spliced nesting will be captured in the water
-- on either side of the island. 
-- (Plugging a handbuilt document is more complicated).
plugParsedTemplate :: ParsedTemplate -> [ODoc] -> String
plugParsedTemplate fpls ys = 
    concatNoSpace $ merge showsWater showsIsland $ knitOnB (,) ys fpls
  where
    showsWater :: String -> ShowS
    showsWater = showString 
    
    showsIsland :: (SrcLoc,ODoc) -> ShowS
    showsIsland (loc,doc) = output (_src_column loc) 80 doc
    
    

  
-- Docs created by hand must be able to generate the appropriate 
-- nesting in output. This is achieved by having /higher-order water/
-- in the FPList - the water is an ODocS function to fit the island.
-- So we need to pass the island to the water with a bimerge.     
plugHandDoc :: FPList ODocS a -> [ODoc] -> String
plugHandDoc fpls ys = 
    concatNoSpace $ bimerge pairwise flush $ knitOnB swap ys fpls
  where
    pairwise :: ODocS -> ODoc -> ShowS
    pairwise f d = output 0 80 (f d)
    
    flush :: ODocS -> ShowS
    flush f = output 0 80 (f emptyDoc)
    
    swap :: a -> b -> b
    swap _ = id
       
  