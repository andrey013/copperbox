
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
import Text.PrettyPrint.Leijen


-- Two representations of a file

-- 1. Source file - preserves source text and holes for filling. 

newtype SourceFile = SourceFile { getchunks :: Seq ScoreChunk } 
  deriving (Show) 
  
data ScoreChunk = SourceText String | MetaMark SrcPos MetaDirective
  deriving (Show)

type DId = Int
type Name = String
type Scheme = String
data MetaDirective = MetaOutput DId Name Scheme
  deriving Show
  
data SrcPos = SrcPos { 
    _src_line     :: Int,
    _src_column   :: Int,
    _src_file     :: String
  }
  deriving (Show)  

-- 2. Partially interpreted into a list of expressions for terms of interest 
-- i.e. time signatures, key signatures; and directives for output 


data SrcExpr = Command Command
             | Directive MetaDirective
             | Nested [SrcExpr]
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


c_major = Key middle_c Major  

four_four = TimeSig 4 4




  
--------------------------------------------------------------------------------
-- Pretty instances

instance Pretty SourceFile where
  pretty (SourceFile se) = F.foldl (\d e -> d <> pretty e) empty se 


instance Pretty ScoreChunk where
  pretty (SourceText str) = string str
  pretty (MetaMark pos d) = enclose (text "{-# ") (text " #-}")
                                    (pretty d)


instance Pretty SrcExpr where
  pretty (Command cmd)    = text "\\cmd" <+> pretty cmd
  pretty (Directive drct) = enclose (text "<-- ") (text " -->") (pretty drct)
  pretty (Nested [])      = text "{ }"   
  pretty (Nested xs)      = enclose (lbrace <> line) (line <> rbrace) 
                                    (indent 2 $ vcat $ map pretty xs)

instance Pretty Command where
  pretty (CmdKey e)               = text "-key" <+> (text . show) e
  pretty (CmdMeter e)             = text "-meter" <+> (text . show) e
  pretty (CmdDefaultNoteLength e) = text "-len" <+> (text . show) e
  pretty (CmdRelativePitch e)     = text "-rpitch" <+> (text . show) e
  
instance Pretty MetaDirective where
  pretty (MetaOutput i name scheme) = 
      prefix i <+> text name <> colon <> text scheme
    where
      prefix i | i > 999    = int i
               | otherwise  = let s = show i; l = 4 - length s;
                              in text $ replicate l '0' ++ s    
                                                