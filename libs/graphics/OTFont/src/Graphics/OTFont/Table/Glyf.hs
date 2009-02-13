{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Glyf
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Glyf - Glyf data
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Glyf where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Parse
import Graphics.OTFont.Pretty

import Text.ZParse

import Control.Applicative
import Data.Array.Unboxed hiding (array)


import Text.PrettyPrint.Leijen ( Pretty(..), Doc, space, (<>), hsep )

data Glyf = 
      SimpleGlyf { 
          gylf_header       :: GlyfHeader,
          simple_glyf_body  :: SimpleGlyph 
        }
    | CompositeGlyf { 
          gylf_header       :: GlyfHeader,
          comp_glyf_body    :: CompositeGlyph 
      }                 
  deriving (Eq,Show)
  
                                  
  
data GlyfHeader = GlyfHeader { 
      num_contours    :: Short,
      glyf_x_min      :: Short,
      glyf_y_min      :: Short,
      glyf_x_max      :: Short,
      glyf_y_max      :: Short
    }
  deriving (Eq,Show)

composite :: GlyfHeader -> Bool
composite (GlyfHeader nc _ _ _ _)     | nc < 0      = True
                                      | otherwise   = False
                                      
readGlyf :: Monad m => ReadData m Glyf
readGlyf = do 
    hdr <- readGlyfHeader
    if composite hdr then do cbdy <- readCompositeGlyph
                             return $ CompositeGlyf hdr cbdy
                     else do sbdy <- readSimpleGlyph (num_contours hdr)
                             return $ SimpleGlyf hdr sbdy
                                      
readGlyfHeader :: Monad m => ReadData m GlyfHeader
readGlyfHeader = GlyfHeader <$>
    short <*> short <*> short <*> short <*> short

instance Pretty Glyf where
  pretty (SimpleGlyf h b) = ppTable "simple glyph" (xs ++ ys) where
      xs = headerFields h
      ys = simpleGlyphFields b
      
      
  pretty (CompositeGlyf h _b) = ppTable "composite glyph" (xs ++ ys) where
      xs = headerFields h
      ys = []

    
headerFields :: GlyfHeader -> [Doc]
headerFields t = 
    [ field "num_contours"      24 (integral $ num_contours t)
    , field "glyf_x_min"        24 (integral $ glyf_x_min t)
    , field "glyf_y_min"        24 (integral $ glyf_y_min t)
    , field "glyf_x_max"        24 (integral $ glyf_x_max t)
    , field "glyf_y_max"        24 (integral $ glyf_y_max t)
    ]




--------------------------------------------------------------------------------
--

data OutlinePoint = OnCurvePt UShort UShort
                | OffCurvePt UShort UShort
  deriving (Eq,Ord,Show)
 

data Coord = Coord UShort UShort
  deriving (Eq,Show)
    

data SimpleGlyph = SimpleGlyph {
      end_pts_of_contours   :: [UShort],
      instruction_length    :: UShort,
      instructions          :: UArray UShort Byte,  -- uninterpreted
      sg_flags              :: Array UShort Byte,
      x_coordinates         :: Array UShort Coord,
      y_coordinates         :: Array UShort Coord
    }
  deriving (Eq,Show)
  
  
data SimpleGlyphFlag  = 
      SG0_OnCurve
    | SG1_XShortVector
    | SG2_YShortVector
    | SG3_Repeat
    | SG4_XSame
    | SG5_YSame
    | SG6_Reserved
    | SG7_Reserved
  deriving (Enum,Eq,Ord,Show)

readSimpleGlyph :: Monad m => Short -> ReadData m SimpleGlyph
readSimpleGlyph nc = do
    ends  <- count (fromIntegral nc) ushort
    let csize = 1 + fromIntegral (foldr max 0 ends)
    
    len   <- ushort
    insts <- uarray len byte
    flags <- array csize byte
    return $ SimpleGlyph ends len insts flags undefined undefined


-- Consume 1 byte to get a UShort
extract1byte :: Monad m => ReadData m UShort
extract1byte = fromIntegral <$> word8

-- Consume 2 bytes to get a UShort
extract2byte :: Monad m => ReadData m UShort
extract2byte = word16be



simpleGlyphFields :: SimpleGlyph -> [Doc]
simpleGlyphFields (SimpleGlyph ends _ _ flags _ _) = 
    [ field "end_pts_of_contours"      24 (hsep $ map ((space <>) . integral) ends)
    , field "flags"                    24 (ppArray ((space <>) . pphex2) flags)
    ]
    
    
--------------------------------------------------------------------------------
--

data CompositeGlyph = CompositeGlyph {
      component_flag    :: Short,
      glyph_index       :: Short
    }
  deriving (Eq,Show)
  
  
data CompositeGlyphFlag  = 
      CG0_ArgsAreWords
    | CG1_XYArgsAreValues
    | CG2_XYRoundToGrid
    | CG3_WeHaveScale
    | CG4_Reserved
    | CG5_MoreComponents
    | CG6_WeHaveXYScale
    | CG7_WeHave2X2
    | CG8_WeHaveInstructions
    | CG9_UseMyMetrics
    | CG10_OverlapCompound 
    | CG11_ScaledomponentOffset
    | CG12_UnscaledComponentOffset
    | CompositeGlyphFlag UShort
  deriving (Eq,Ord,Show)
  
readCompositeGlyph :: Monad m => ReadData m CompositeGlyph
readCompositeGlyph = undefined

