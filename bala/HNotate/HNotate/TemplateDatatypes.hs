
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
  

data OutputScheme = OutputRelative | OutputDefault
  deriving (Eq,Show)
  
  
  

-- Two views of a file

-- 1. Text view - (Water,Hole)
-- Preserves source text - the holes are collected at their 
-- source position for filling. 

type TextChunk = (String, Maybe SrcPos)

 

-- 2. Expression view
-- Expressions of interest (e.g. time signatures, key signatures)
-- and meta comments, are extracted from the original file (via a 
-- preprocessing step). Everything else is dropped. 
--
-- LilyPond's nesting structure is respected, Abc has an artificial 
-- nesting structure 'manufactured'). 


data Expr = Expr Term [Expr]
  deriving (Eq,Show) 
  
  
data Term = Let Binding
          | OutputDirective (Maybe OutputScheme) String
  deriving (Eq,Show)            

data Binding = LetCadenza Bool
             | LetKey Key
             | LetMeter Meter
             | LetMeterPattern MeterPattern
             | LetPartial Duration
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
    error $ "Malformed Term - #output " ++ name ++ " " ++ show xs

convBinding :: Binding -> (Env -> Env)
convBinding (LetCadenza b)        = set_cadenza b
convBinding (LetKey k)            = set_current_key k
convBinding (LetMeter m)          = set_current_meter m
convBinding (LetMeterPattern mp)  = set_meter_pattern mp
convBinding (LetPartial d)        = set_partial_measure d
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



  
