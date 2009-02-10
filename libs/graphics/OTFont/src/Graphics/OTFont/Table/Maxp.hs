{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Maxp
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Maxp - maximum profile
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Maxp where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Parse
import Graphics.OTFont.Pretty
import Text.ZParse

import Control.Applicative
import Data.Word

import Text.PrettyPrint.Leijen ( Pretty(..) )

--------------------------------------------------------------------------------
-- Version 0.5

data MaxpTable_0_5 = MaxpTable_0_5 { 
      maxp_0_5_version_num    :: Fixed,
      maxp_0_5_num_glyphs     :: Word16
    }
  deriving (Eq,Show)

readMaxpTable_0_5 :: Monad m => ReadData m MaxpTable_0_5
readMaxpTable_0_5 = MaxpTable_0_5 <$> 
    undefined <*> undefined
    
instance Pretty MaxpTable_0_5 where
  pretty t = ppTable "maxp Table (version 0.5)"
      [ field "version_num"       24 (pretty   $ maxp_0_5_version_num t)
      , field "num_glyphs"        24 (integral $ maxp_0_5_num_glyphs t)
      ]

--------------------------------------------------------------------------------
-- Version 1.0

data MaxpTable_1_0 = MaxpTable_1_0 { 
      maxp_1_0_version_num      :: Fixed,
      maxp_1_0_num_glyphs       :: Word16,
      max_points                :: Word16,
      max_contours              :: Word16,
      max_composite_points      :: Word16,
      max_composite_contours    :: Word16,
      max_zones                 :: Word16,
      max_twilight_points       :: Word16,
      max_storage               :: Word16,
      max_function_defs         :: Word16,
      max_instruction_defs      :: Word16,
      max_stack_elements        :: Word16,
      max_size_of_instructions  :: Word16,
      max_component_elements    :: Word16,
      max_component_depth       :: Word16
    }
  deriving (Eq,Show)

readMaxpTable_1_0 :: Monad m => ReadData m MaxpTable_1_0
readMaxpTable_1_0 = MaxpTable_1_0 <$> 
        fixed     <*> ushort  <*> ushort  <*> ushort
    <*> ushort    <*> ushort  <*> ushort  <*> ushort
    <*> ushort    <*> ushort  <*> ushort  <*> ushort
    <*> ushort    <*> ushort  <*> ushort
    
    
instance Pretty MaxpTable_1_0 where
  pretty t = ppTable "maxp Table (version 1.0)"
      [ field "version_num"               24 (pretty   $ maxp_1_0_version_num t)
      , field "num_glyphs"                24 (integral $ maxp_1_0_num_glyphs t)
      , field "max_points"                24 (integral $ max_points t)
      , field "max_contours"              24 (integral $ max_contours t)
      , field "max_composite_points"      24 (integral $ max_composite_points t)
      , field "max_composite_contours"    24 (integral $ max_composite_contours t)
      , field "max_zones"                 24 (integral $ max_zones t)
      , field "max_twilight_points"       24 (integral $ max_twilight_points t)
      , field "max_storage"               24 (integral $ max_storage t)
      , field "max_function_defs"         24 (integral $ max_function_defs t)
      , field "max_instruction_defs"      24 (integral $ max_instruction_defs t)
      , field "max_stack_elements"        24 (integral $ max_stack_elements t)
      , field "max_size_of_instructions"  24 (integral $ max_size_of_instructions t)
      , field "max_component_elements"    24 (integral $ max_component_elements t)
      , field "max_component_depth"       24 (integral $ max_component_depth t)
      ]


