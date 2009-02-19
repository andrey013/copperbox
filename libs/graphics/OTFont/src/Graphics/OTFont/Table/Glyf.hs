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
import Graphics.OTFont.ParseMonad
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty


import Control.Applicative


import Text.PrettyPrint.Leijen ( Pretty(..), Doc, space, (<>), hsep )

data Glyf = 
      SimpleGlyf { 
          gylf_header       :: GlyfHeader,
          simple_glyf_body  :: SimpleGlyphData 
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
                                      
readGlyf :: Monad m => Int -> Int -> ParserT r m Glyf
readGlyf offset len = withinRangeRel offset len $ do 
    hdr <- readGlyfHeader
    if composite hdr then do cbdy <- readCompositeGlyph
                             return $ CompositeGlyf hdr cbdy
                     else do sbdy <- readSimpleGlyphData (num_contours hdr)
                             return $ SimpleGlyf hdr sbdy
                                      
readGlyfHeader :: Monad m => ParserT r m GlyfHeader
readGlyfHeader = GlyfHeader <$>
    short <*> short <*> short <*> short <*> short

instance Pretty Glyf where
  pretty (SimpleGlyf h b) = ppTable "simple glyph" (xs ++ ys) where
      xs = headerFields h
      ys = simpleGlyphDataFields b
      
      
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


 

data Coord = Coord UShort UShort
  deriving (Eq,Show)
    

data SimpleGlyphData = SimpleGlyphData {
      end_pts_of_contours   :: [UShort],
      instruction_length    :: UShort,
      instructions          :: USequence Byte,  -- uninterpreted
      sg_flags              :: [Byte],
      xy_data               :: [Byte]
    }
  deriving (Eq,Show)
  
  


readSimpleGlyphData :: Monad m => Short -> ParserT r m SimpleGlyphData
readSimpleGlyphData nc = do
    ends  <- count (fromIntegral nc) ushort
    let csize = 1 + fromIntegral (foldr max 0 ends)
    len   <- ushort
    insts <- usequence (fromIntegral len) byte
    flags <- count csize byte
    xy    <- runOnL byte
    return $ SimpleGlyphData ends len insts flags xy






simpleGlyphDataFields :: SimpleGlyphData -> [Doc]
simpleGlyphDataFields (SimpleGlyphData ends _ _ flags _) = 
    [ field "end_pts_of_contours"      24 (hsep $ map ((space <>) . integral) ends)
    , field "flags"                    24 (hsep $ map pphex2 flags)
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
  
readCompositeGlyph :: Monad m => ParserT r m CompositeGlyph
readCompositeGlyph = undefined

