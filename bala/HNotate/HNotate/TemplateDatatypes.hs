
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
-- Datatypes for elements extracted from score templates.
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
  
type TextChunk = (String, Maybe SrcPos)

  
type Idx = Int

type Name = String

type SystemIndex = String

data MetaDirective = MetaOutput OutputScheme SystemIndex
                   | MetaMeter MeterPattern
  deriving Show

data OutputScheme = OutputRelative | OutputDefault
  deriving (Eq,Show)
  
  
  

-- Two views of a file

-- 1. Source preserving view 
-- Preserves source text - the holes are indexed for filling. 

newtype TextualView = TextualView { getTextElements :: Seq TextElement } 
  deriving (Show) 
  
data TextElement = SourceText String | MetaMark Idx SrcPos String
  deriving (Show)

 

-- 2. Expression view
-- Expressions of interest (e.g. time signatures, key signatures)
-- and meta comments, are extracted from the original file (via a 
-- preprocessing step). Everything else is dropped. 
--
-- LilyPond's nesting structure is respected, Abc has an 
-- artificial nesting structure 'manufactured'). 


data Expr = Expr Term [Expr]
  deriving (Eq,Show) 
  
  
data Term = Let Binding
          | OutputDirective (Maybe OutputScheme) String
  deriving (Eq,Show)            

data Binding = LetMeter Meter
             | LetKey Key
             | LetMeterPattern MeterPattern
             | LetRelativePitch Pitch
             | LetNone                -- Used for 'X' fields in Abc
  deriving (Eq,Show)             

newtype Hoas = Hoas { getExprs :: [HoasExpr] }
  deriving (Show)

data HoasExpr = HLetExpr (Env -> Env) [HoasExpr]
              | HOutputDirective (Maybe OutputScheme) String
              

toHoas :: [Expr] -> Hoas
toHoas xs = Hoas $ map convExpr xs

convExpr :: Expr -> HoasExpr
convExpr (Expr (Let b) es)                      = 
    HLetExpr (convBinding b) (map convExpr es)
    
convExpr (Expr (OutputDirective oscm name) [])  =
    HOutputDirective oscm name 

convExpr (Expr (OutputDirective oscm name) xs)  =
    error $ "Malformed Term - this is a bug please report"

convBinding :: Binding -> (Env -> Env)
convBinding (LetMeter m)          = set_current_meter m
convBinding (LetKey k)            = set_current_key k
convBinding (LetMeterPattern mp)  = set_meter_pattern mp
convBinding (LetRelativePitch p)  = set_relative_pitch p
convBinding (LetNone)             = id

instance Show HoasExpr where
  show (HLetExpr fn xs)                     = "let <fun> in " ++ show xs
  show (HOutputDirective Nothing name)      = "# output: " ++ name
  show (HOutputDirective (Just scm) name)   = 
      "# output: \\" ++ schemeName scm ++ " " ++ name
    where
       schemeName OutputRelative  = "relative" 
       schemeName OutputDefault   = "default"                  



  
