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
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty

import Control.Applicative

import Text.PrettyPrint.Leijen ( Pretty(..), Doc )

--------------------------------------------------------------------------------
-- Version 0.5

data MaxpTable = 
      MaxpTable_0_5 { 
          version_num    :: Fixed, 
          num_glyphs     :: UShort  
      }
    | MaxpTable_1_0 { 
          version_num    :: Fixed,
          num_glyphs     :: UShort,
          maxp_1_0_body  :: Version_1_0_Body 
      }
  deriving (Eq,Show)
               


readMaxpTable :: ParserM r MaxpTable
readMaxpTable = do 
    v   <- fixed 
    ng  <- ushort
    if v == 1.0 then do b <- readVersion1Body
                        return $ MaxpTable_1_0 v ng b
                else return $ MaxpTable_0_5 v ng 
    


--------------------------------------------------------------------------------
-- Version 1.0

data Version_1_0_Body = Version_1_0_Body {
      max_points                :: UShort,
      max_contours              :: UShort,
      max_composite_points      :: UShort,
      max_composite_contours    :: UShort,
      max_zones                 :: UShort,
      max_twilight_points       :: UShort,
      max_storage               :: UShort,
      max_function_defs         :: UShort,
      max_instruction_defs      :: UShort,
      max_stack_elements        :: UShort,
      max_size_of_instructions  :: UShort,
      max_component_elements    :: UShort,
      max_component_depth       :: UShort
    }
  deriving (Eq,Show)

readVersion1Body :: ParserM r Version_1_0_Body
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
      
      
  pretty (MaxpTable_1_0 v g b) = ppTable "maxp Table (version 1.0)" xs
      where
        xs = x1 : x2 : fieldlist b
        x1 = field "version_num"       24 (pretty v)
        x2 = field "num_glyphs"        24 (integral g)

fieldlist :: Version_1_0_Body -> [Doc]
fieldlist b = 
    [ field "max_points"                24 (integral $ max_points b)
    , field "max_contours"              24 (integral $ max_contours b)
    , field "max_composite_points"      24 (integral $ max_composite_points b)
    , field "max_composite_contours"    24 (integral $ max_composite_contours b)
    , field "max_zones"                 24 (integral $ max_zones b)
    , field "max_twilight_points"       24 (integral $ max_twilight_points b)
    , field "max_storage"               24 (integral $ max_storage b)
    , field "max_function_defs"         24 (integral $ max_function_defs b)
    , field "max_instruction_defs"      24 (integral $ max_instruction_defs b)
    , field "max_stack_elements"        24 (integral $ max_stack_elements b)
    , field "max_size_of_instructions"  24 (integral $ max_size_of_instructions b)
    , field "max_component_elements"    24 (integral $ max_component_elements b)
    , field "max_component_depth"       24 (integral $ max_component_depth b)
    ]



    