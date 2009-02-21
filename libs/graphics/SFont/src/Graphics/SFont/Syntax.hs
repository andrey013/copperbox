{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.Syntax
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Font file structured as a hierarchical syntax tree, rather than a 
-- graph of tables.
--
--------------------------------------------------------------------------------


module Graphics.SFont.Syntax where

import Graphics.SFont.PrimitiveDatatypes

-- | a TT or OT font file
data TTFF = TTFF 
      { sfnt_version      :: SfntVersion
      , number_of_tables  :: Int
      , font_header       :: FontHeader
      
      , glyphs            :: [Glyph] 
      }
  deriving (Eq,Show)

data SfntVersion = 
        Sfnt_1_0
      | OTTO  
  deriving (Enum,Eq,Ord,Show) 

--------------------------------------------------------------------------------
-- head table 

data FontHeader = FontHeader
        { table_version_num       :: Fixed
        , font_revision           :: Fixed
        , check_sum_adjust        :: ULong
        , magic_number            :: ULong
        , head_flags              :: [HeadFlag]
        , units_per_em            :: UShort
        , created_timestamp       :: DateTime
        , modified_timestamp      :: DateTime
        , max_bb                  :: BoundingBox
        , mac_style               :: [MacStyle]
        , smallest_readable_size  :: UShort
        , font_direction_hint     :: Short      -- this could be a type...
        , index_to_loc_format     :: Short
        , glyph_data_format       :: Short
        }
  deriving (Eq,Show)

  
data HeadFlag = 
      H0_Baseline_at_y
    | H1_Left_sidebearing_at_x
    | H2_Depends_pt_size
    | H3_Force_ppem_int
    | H4_Alter_adv_width
    | H5_Undefined
    | H6_Undefined
    | H7_Undefined
    | H8_Undefined
    | H9_Undefined 
    | H10_Undefined    
    | H11_Lossless_compression
    | H12_Converted
    | H13_ClearType_optimised
    | H14_Reserved
    | H15_Reserved  
  deriving (Enum,Eq,Ord,Show)
  
    
data MacStyle = 
      S0_Bold
    | S1_Italic  
    | S2_Underline
    | S3_Outline
    | S4_Shadow
    | S5_Condensed
    | S6_Extended
    | S7_Reserved
    | S8_Reserved
    | S9_Reserved
    | S10_Reserved
    | S11_Reserved
    | S12_Reserved
    | S13_Reserved
    | S14_Reserved
    | S15_Reserved
  deriving (Enum,Eq,Ord,Show)
  
--------------------------------------------------------------------------------
-- glyf table 

type GlyphName = String 

data Glyph = 
      SimpleGlyph     GlyphName BoundingBox [Contour]
    | CompositeGlyph  GlyphName BoundingBox
  deriving (Eq,Show)    

             
newtype Contour = Contour { getContour :: [OutlinePoint] }
  deriving (Eq,Show)

data OutlinePoint = OnCurvePt  Short Short
                  | OffCurvePt Short Short
  deriving (Eq,Ord,Show)


  
-- Bounding box - do all bounding boxes in a font file (tt or ot)  
-- use short for the dimension? 
-- @head@ - does
-- @glyf@ - does
 
data BoundingBox = BoundingBox 
        { x_min   :: Short
        , y_min   :: Short
        , x_max   :: Short
        , y_max   :: Short
        }
  deriving (Eq,Show)     