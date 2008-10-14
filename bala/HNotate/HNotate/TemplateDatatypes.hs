
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

type Idx = Int

type Name = String

type SystemIndex = String

data MetaDirective = MetaOutput OutputScheme SystemIndex
                   | MetaMeter MeterPattern
  deriving Show

data OutputScheme = LyRelative | AbcDefault
  deriving Show

-- Two views of a file

-- 1. Source preserving view 
-- Preserves source text - the holes are indexed for filling. 

newtype TextualView = TextualView { getTextElements :: Seq TextElement } 
  deriving (Show) 
  
data TextElement = SourceText String | MetaMark Idx SrcPos String
  deriving (Show)



  
data SrcPos = SrcPos { 
    _src_line     :: Int,
    _src_column   :: Int,
    _src_file     :: String
  }
  deriving (Show)  

-- 2. Expression view
-- Expressions of interest (e.g. time signatures, key signatures)
-- and meta comments, are extracted from the original file (via a 
-- preprocessing step). Everything else is dropped. 
--
-- LilyPond's nesting structure is respected, Abc has an 
-- artificial nesting structure 'manufactured'). 
newtype ExprView = ExprView { getExprs :: [Expr] }
  deriving (Show)

data Expr = LetExpr (Env -> Env) [Expr]
          | Action Idx MetaDirective

instance Show Expr where
  show (LetExpr fn xs) = "let <fun> in " ++ show xs
  show (Action i md)   = "#" ++ show i ++ ":" ++ show md
                      

-- Plugs fill holes in a source preserving view 
data Plug = Plug Idx Doc

  
