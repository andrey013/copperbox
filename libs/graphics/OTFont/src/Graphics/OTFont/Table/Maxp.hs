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

import Control.Applicative
import Data.Word

import Text.PrettyPrint.Leijen ( Pretty(..), Doc )

--------------------------------------------------------------------------------
-- Version 0.5

data MaxpTable = MaxpTable_0_5 { version_num :: Fixed, 
                                 num_glyphs  :: Word16  }
                               
               | MaxpTable_1_0 { version_num :: Fixed,
                                 num_glyphs  :: Word16,
                                 body        :: Version_1_0_Body }
  deriving (Eq,Show)
               


readMaxpTable :: Monad m => ReadData m MaxpTable
readMaxpTable = do 
    v   <- fixed 
    ng  <- ushort
    if v == 1.0 then do body <- readVersion1Body
                        return $ MaxpTable_1_0 v ng body
                else return $ MaxpTable_0_5 v ng 
    


--------------------------------------------------------------------------------
-- Version 1.0

data Version_1_0_Body = Version_1_0_Body {
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

readVersion1Body :: Monad m => ReadData m Version_1_0_Body
readVersion1Body = Version_1_0_Body <$>  
        ushort    <*> ushort  <*> ushort  <*> ushort
    <*> ushort    <*> ushort  <*> ushort  <*> ushort
    <*> ushort    <*> ushort  <*> ushort  <*> ushort
    <*> ushort
    
    
instance Pretty MaxpTable where
  pretty (MaxpTable_0_5 v g) = ppTable "maxp Table (version 0.5)"
      [ field "version_num"       24 (pretty   v)
      , field "num_glyphs"        24 (integral g)
      ]
      
      
  pretty (MaxpTable_1_0 v g body) = ppTable "maxp Table (version 1.0)" xs
      where
        xs = x1 : x2 : fieldlist body
        x1 = field "version_num"       24 (pretty v)
        x2 = field "num_glyphs"        24 (integral g)

fieldlist :: Version_1_0_Body -> [Doc]
fieldlist body = 
    [ field "max_points"                24 (integral $ max_points body)
    , field "max_contours"              24 (integral $ max_contours body)
    , field "max_composite_points"      24 (integral $ max_composite_points body)
    , field "max_composite_contours"    24 (integral $ max_composite_contours body)
    , field "max_zones"                 24 (integral $ max_zones body)
    , field "max_twilight_points"       24 (integral $ max_twilight_points body)
    , field "max_storage"               24 (integral $ max_storage body)
    , field "max_function_defs"         24 (integral $ max_function_defs body)
    , field "max_instruction_defs"      24 (integral $ max_instruction_defs body)
    , field "max_stack_elements"        24 (integral $ max_stack_elements body)
    , field "max_size_of_instructions"  24 (integral $ max_size_of_instructions body)
    , field "max_component_elements"    24 (integral $ max_component_elements body)
    , field "max_component_depth"       24 (integral $ max_component_depth body)
    ]



    