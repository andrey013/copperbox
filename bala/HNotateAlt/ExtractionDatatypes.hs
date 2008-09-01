
--------------------------------------------------------------------------------
-- |
-- Module      :  ExtractionDatatypes
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

module ExtractionDatatypes where

import Duration
import Pitch

import Data.Ratio
import Data.Sequence hiding (empty) 
import Text.PrettyPrint.Leijen


-- Two representations of a file

-- 1. Source file - preserves source text and holes for filling. 

newtype SourceFile = SourceFile { getchunks :: Seq ScoreChunk } 
  deriving (Show) 
  
data ScoreChunk = SourceText String | MetaMark SrcPos MetaDirective
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

-- 2. Partially interpreted into a list of expressions for terms of interest 
-- i.e. time signatures, key signatures; and directives for output 


data SrcExpr = Commmand Command
             | Directive String
             | Nested [SrcExpr]
           --  | WaterExpr
  deriving Show                 
                 


data Command = CmdKey Key
             | CmdMeter Meter
             | CmdDefaultNoteLength Duration  -- Abc only
             | CmdRelativePitch Pitch         -- Lilypond only
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


  


  
--------------------------------------------------------------------------------
-- Pretty instances


instance Pretty SrcExpr where
  pretty (Commmand cmd)   = text "\\cmd" <+> pretty cmd
  pretty (Directive str)  = enclose (text "<-- ") (text " -->") (text str)
  pretty (Nested [])      = text "{ }"   
  pretty (Nested xs)      = enclose (lbrace <> line) (line <> rbrace) 
                                    (indent 2 $ vcat $ map pretty xs)

instance Pretty Command where
  pretty (CmdKey e)               = text "-key" <+> (text . show) e
  pretty (CmdMeter e)             = text "-meter" <+> (text . show) e
  pretty (CmdDefaultNoteLength e) = text "-len" <+> (text . show) e
  pretty (CmdRelativePitch e)     = text "-rpitch" <+> (text . show) e
                                                