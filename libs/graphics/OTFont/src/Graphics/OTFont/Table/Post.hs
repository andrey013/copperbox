{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Post
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Post - PostScript
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Post where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.ParseMonad
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty
import Graphics.OTFont.Utils

import Control.Applicative
import Data.Typeable

import Text.PrettyPrint.Leijen ( Pretty(..), vsep, (<+>), string  )

data PostTable = PostTable { 
      post_version        :: Fixed,
      italic_angle        :: Fixed,
      underline_position  :: FWord,
      underline_thickness :: FWord,
      is_fixed_pitch      :: ULong,      -- c.f. IntegralBool
      mem_usage           :: PostMemUsage,
      post_subtable       :: PostSubtable
    }
  deriving (Eq,Show,Typeable)

data PostMemUsage = PostMemUsage {
      min_mem_type42      :: ULong,
      max_mem_type42      :: ULong,
      min_mem_type1       :: ULong,
      max_mem_type1       :: ULong
    }
  deriving (Eq,Show)

data PostSubtable = 
      NoPostSubtable      -- for table versions 1.0 and 3.0   
    | Version2_0 { 
          number_of_glyphs      :: UShort,
          glyph_name_index      :: USequence UShort,
          names                 :: [String]
      }
    | UnrecognizedPostSubtable Fixed
  deriving (Eq,Show)
  
readPostTable :: Monad m => ParserT r m PostTable
readPostTable = do 
    v     <- fixed
    b     <- fixed   
    c     <- fword
    d     <- fword
    e     <- ulong  
    f     <- readPostMemUsage
    g     <- readPostSubtable v
    return $ PostTable v b c d e f g

readPostMemUsage :: Monad m => ParserT r m PostMemUsage
readPostMemUsage = PostMemUsage <$>
        ulong <*> ulong <*> ulong <*> ulong 


readPostSubtable :: Monad m => Fixed -> ParserT r m PostSubtable
readPostSubtable d 
    | d == 1.0 || d == 3.0  = return NoPostSubtable
    | d == 2.0              = do n    <- ushort
                                 arr  <- usequence (fromIntegral n) ushort
                                 cs   <- runOnL pascalString
                                 return $ Version2_0 n arr cs
    | otherwise             = return $ UnrecognizedPostSubtable d
        
    
instance Pretty PostTable where
  pretty t = ppTable "Post Table"
      [ field "version"             24 (pretty   $ post_version t)
      , field "italic_angle"        24 (pretty   $ italic_angle t)
      , field "underline_position"  24 (pretty   $ underline_position t)
      , field "underline_thickness" 24 (pretty   $ underline_thickness t)
      , field "is_fixed_pitch"      24 (pretty   $ boolValue $ is_fixed_pitch t)
      , field "min_mem_type42"      24 (integral $ max_mem_type1 $ mem_usage t) 
      , field "max_mem_type42"      24 (integral $ max_mem_type1 $ mem_usage t) 
      , field "min_mem_type1"       24 (integral $ max_mem_type1 $ mem_usage t) 
      , field "max_mem_type1"       24 (integral $ max_mem_type1 $ mem_usage t)
      , field "subtable"            24 (pretty $ post_subtable t) 
      ]

instance Pretty PostSubtable where 
  pretty NoPostSubtable               = string "no subtable"
  pretty (Version2_0 n a ss)          = ppTable "Version 2.0 subtable"
      [ field "number_of_glyphs"    24 (integral n)
      , field "glyph_name_index"    24 (ppArray integral a)
      , field "names"               24 (vsep $ map pretty ss)
      ]
  pretty (UnrecognizedPostSubtable v) = string "Unrecognized subtable " 
                                        <+> pretty v

    
                  