{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.QueryDC
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Querying the Drawing Context.
--
-- \*\* WARNING \*\* - parts of this module especially the 
-- mono-space glyph metrics need a re-think and will change or be 
-- dropped.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.QueryDC
  ( 

    dsize 
  , dsizeF
  , usize
  , usizeF
  , castsize
  , castsizeF

  , textAttr
  , strokeAttr
  , fillAttr
  , borderedAttr

  , position
  , snapmove

  , roundCornerSizeAU
  , roundCornerSizeRU
  , textMarginAU
  , textMarginRU

  , getLineWidth
  , getFontAttr
  , getFontSize
  , getFontFace
  , getTextColour

  , markHeightAU
  , markHeightRU
  , markHalfHeightAU
  , markHalfHeightRU
  , baselineSpacing

  -- * Glyph metrics
  , glyphBoundingBoxAU
  , glyphBoundingBoxRU
  , capHeightAU
  , capHeightRU
  , descenderAU
  , descenderRU
  , verticalSpanAU
  , verticalSpanRU

  , cwLookupTable


  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.FontMetrics

import Wumpus.Core                              -- package: wumpus-core
import qualified Wumpus.Core.FontSize   as FS
 
import Control.Applicative
import Control.Monad


dsize :: (DrawingCtxM m, CtxSize u) => u -> m Double
dsize u = (\sz -> cfSize sz u) <$> query dc_font_size

dsizeF :: (Functor t, DrawingCtxM m, CtxSize u) => t u -> m (t Double)
dsizeF obj = (\sz -> fmap (cfSize sz) obj) <$> query dc_font_size

usize :: (DrawingCtxM m, CtxSize u) => Double -> m u
usize u = (\sz -> csSize sz u) <$> query dc_font_size

usizeF :: (Functor t, DrawingCtxM m, CtxSize u) => t Double -> m (t u)
usizeF obj = (\sz -> fmap (csSize sz) obj) <$> query dc_font_size

castsize :: (DrawingCtxM m, CtxSize u, CtxSize u1) => u -> m u1
castsize = dsize >=> usize

castsizeF :: (Functor f, DrawingCtxM m, CtxSize u, CtxSize u1) 
          => f u -> m (f u1)
castsizeF = dsizeF >=> usizeF


textAttr :: DrawingCtxM m => m (RGBi,FontAttr)
textAttr = (,) <$> query dc_text_colour <*> getFontAttr


strokeAttr :: DrawingCtxM m => m (RGBi, StrokeAttr)
strokeAttr = (,) <$> query dc_stroke_colour <*> query dc_stroke_props


fillAttr :: DrawingCtxM m => m RGBi
fillAttr = query dc_fill_colour


borderedAttr :: DrawingCtxM m => m (RGBi, StrokeAttr, RGBi)
borderedAttr = (,,) <$> query dc_fill_colour <*> query dc_stroke_props 
                                             <*> query dc_stroke_colour



-- | Get the Point corresponding the grid coordinates scaled by
-- the snap-grid scaling factors.
--
-- Absolute units.
-- 
position :: (Fractional u, PsDouble u, DrawingCtxM m) 
         => (Int,Int) -> m (Point2 u)
position (x,y) = post <$> query dc_snap_grid_factors
  where
    post (scx,scy) = P2 (fromPsDouble $ scx * fromIntegral x) 
                        (fromPsDouble $ scy * fromIntegral y)


-- | Scale a vector coordinate by the snap-grid scaling factors.
--
-- Absolute units.
--
snapmove :: (Fractional u, PsDouble u, DrawingCtxM m) 
         => (Int,Int) -> m (Vec2 u)
snapmove (x,y) = post <$> query dc_snap_grid_factors
  where
    post (scx,scy) = V2 (fromPsDouble $ scx * fromIntegral x) 
                        (fromPsDouble $ scy * fromIntegral y)



-- | Size of the round corner factor.
--
-- Relative units.
--
roundCornerSizeAU :: (Fractional u, PsDouble u , DrawingCtxM m) => m u
roundCornerSizeAU = fromPsDouble <$> query dc_round_corner_factor

-- | Size of the round corner factor.
--
-- Relative units.
--
roundCornerSizeRU :: (Fractional u, CtxSize u , DrawingCtxM m) => m u
roundCornerSizeRU = query dc_round_corner_factor >>= usize


-- | Get the (x,y) margin around text.
--
-- Note - not all text operations in Wumpus are drawn with text 
-- margin. 
-- 
textMarginAU :: (Fractional u, PsDouble u, DrawingCtxM m) => m (u,u)
textMarginAU = query dc_text_margin >>= post
  where
    post (TextMargin xem yem) = dsize xem >>= \x -> dsize yem >>= \y ->
                                return (fromPsDouble x, fromPsDouble y)

    -- cannot use castsizeF on a pair


-- | Get the (x,y) margin around text.
--
-- Note - not all text operations in Wumpus are drawn with text 
-- margin. 
-- 
textMarginRU :: (Fractional u, CtxSize u, DrawingCtxM m) => m (u,u)
textMarginRU = query dc_text_margin >>= post
  where
    post (TextMargin xsep ysep) = (,) <$> castsize xsep <*> castsize ysep

    -- cannot use castsizeF on a pair


getLineWidth :: DrawingCtxM m => m Double
getLineWidth = line_width <$> query dc_stroke_props

getFontAttr :: DrawingCtxM m => m FontAttr
getFontAttr = FontAttr <$> query dc_font_size <*> query dc_font_face


-- These are not especially convenient now the drawing context 
-- has changed...
getFontSize :: DrawingCtxM m => m Int
getFontSize = query dc_font_size

getFontFace :: DrawingCtxM m => m FontFace
getFontFace = query dc_font_face

getTextColour :: DrawingCtxM m => m RGBi
getTextColour = query dc_text_colour


-- | The /mark/ height is the height of a lowercase letter in the 
-- Courier font at the current point size.
--
-- Arrowheads, dots etc. should generally be drawn at the mark 
-- height.
-- 
markHeightAU :: (PsDouble u, DrawingCtxM m) => m u
markHeightAU = fromPsDouble <$> query (FS.xcharHeight . dc_font_size)



-- | Relative unit version of 'markHeightAU'.
-- 
markHeightRU :: (CtxSize u, DrawingCtxM m) => m u
markHeightRU = query (FS.xcharHeight . dc_font_size) >>= usize


markHalfHeightAU :: (Fractional u, PsDouble u, DrawingCtxM m) => m u
markHalfHeightAU = (0.5*) <$> markHeightAU


markHalfHeightRU :: (Fractional u, CtxSize u, DrawingCtxM m) => m u
markHalfHeightRU = (0.5*) <$> markHeightRU



-- | Vertical distance between baselines of consecutive text 
-- lines.
--
-- This is a /scaling factor/ hence there is no absolute or
-- relative unit distinction.
-- 
baselineSpacing :: (Fractional u, DrawingCtxM m) => m u
baselineSpacing = 
    (\sz factor -> realToFrac $ factor * fromIntegral sz)
      <$> query dc_font_size  <*> query dc_line_spacing_factor




--------------------------------------------------------------------------------

glyphQuery :: DrawingCtxM m 
           => (FontMetrics -> FontSize -> a) -> m a
glyphQuery fn = (\ctx -> withFontMetrics fn ctx) <$> queryCtx



-- | Get the font bounding box - this is the maximum boundary of 
-- the glyphs in the font. The span of the height is expected to 
-- be bigger than the cap_height plus descender depth.
--
glyphBoundingBoxAU :: (PsDouble u, DrawingCtxM m) => m (BoundingBox u)
glyphBoundingBoxAU = fmap fromPsDouble <$> glyphQuery get_bounding_box


-- | Get the font bounding box - this is the maximum boundary of 
-- the glyphs in the font. The span of the height is expected to 
-- be bigger than the cap_height plus descender depth.
--
glyphBoundingBoxRU :: (CtxSize u, DrawingCtxM m) => m (BoundingBox u)
glyphBoundingBoxRU = glyphQuery get_bounding_box >>= usizeF


-- | Height of a capital letter.
--
capHeightAU :: (PsDouble u, DrawingCtxM m) => m u
capHeightAU = fromPsDouble <$> glyphQuery get_cap_height


-- | Height of a capital letter.
--
capHeightRU :: (CtxSize u, DrawingCtxM m) => m u
capHeightRU = glyphQuery get_cap_height >>= usize

-- | Note - descender is expected to be negative.
--
descenderAU :: (PsDouble u, DrawingCtxM m) => m u
descenderAU = fromPsDouble <$> glyphQuery get_descender

-- | Note - descender is expected to be negative.
--
descenderRU :: (CtxSize u, DrawingCtxM m) => m u
descenderRU = glyphQuery get_descender >>= usize

-- | This is the distance from cap_height to descender.
--
verticalSpanAU :: (PsDouble u, DrawingCtxM m) => m u
verticalSpanAU = 
    (\ch dd -> ch - dd) <$> capHeightAU <*> descenderAU


-- | This is the distance from cap_height to descender.
--
verticalSpanRU :: (CtxSize u, DrawingCtxM m) => m u
verticalSpanRU = 
    (\ch dd -> ch - dd) <$> capHeightRU <*> descenderRU


-- | Note the CharWidthLookup is not parameteric on units.
--
-- /CharWidth/ is always Double representing PostScript points.
-- Client code must convert this value accordingly.
--
cwLookupTable :: DrawingCtxM m => m CharWidthLookup
cwLookupTable = glyphQuery get_cw_table

