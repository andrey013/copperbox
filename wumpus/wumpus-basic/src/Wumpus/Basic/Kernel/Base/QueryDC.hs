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

    
    textAttr
  , withTextAttr

  , strokeAttr
  , withStrokeAttr

  , fillAttr
  , withFillAttr

  , borderedAttr
  , withBorderedAttr

  , position
  , snapmove

  , getRoundCornerSize
  , getTextMargin

  , getLineWidth
  , getFontAttr
  , getFontSize
  , getFontFace
  , getTextColour

  , markHeight
  , markHalfHeight
  , baselineSpacing

  -- * Glyph metrics
  , glyphBoundingBox
  , glyphCapHeight
  , glyphDescender
  , glyphVerticalSpan
  , cwLookupTable


  ) where

import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.FontMetrics
import Wumpus.Basic.Kernel.Base.Units

import Wumpus.Core                              -- package: wumpus-core
import qualified Wumpus.Core.FontSize   as FS
 
import Control.Applicative


textAttr :: DrawingCtxM m => m (RGBi,FontAttr)
textAttr = (,) <$> query dc_text_colour <*> getFontAttr

-- | Because @textAttr@ is so commonly used here is a functional
-- version that avoids tupling.
--
withTextAttr :: DrawingCtxM m => (RGBi -> FontAttr -> a) -> m a
withTextAttr fn = fn <$> query dc_text_colour <*> getFontAttr


strokeAttr :: DrawingCtxM m => m (RGBi, StrokeAttr)
strokeAttr = (,) <$> query dc_stroke_colour <*> query dc_stroke_props

withStrokeAttr :: DrawingCtxM m => (RGBi -> StrokeAttr -> a) -> m a
withStrokeAttr fn = fn <$> query dc_stroke_colour <*> query dc_stroke_props


fillAttr :: DrawingCtxM m => m RGBi
fillAttr = query dc_fill_colour

withFillAttr :: DrawingCtxM m => (RGBi -> a) -> m a
withFillAttr fn = fn <$> query dc_fill_colour

borderedAttr :: DrawingCtxM m => m (RGBi, StrokeAttr, RGBi)
borderedAttr = (,,) <$> query dc_fill_colour <*> query dc_stroke_props 
                                             <*> query dc_stroke_colour

withBorderedAttr :: DrawingCtxM m => (RGBi -> StrokeAttr -> RGBi -> a) -> m a
withBorderedAttr fn = 
    fn <$> query dc_fill_colour <*> query dc_stroke_props 
                                <*> query dc_stroke_colour



-- | Get the Point corresponding the grid coordinates scaled by
-- the snap-grid scaling factors.
--
position :: (Fractional u, PtSize u, DrawingCtxM m) 
         => (Int,Int) -> m (Point2 u)
position (x,y) = fn <$> query dc_snap_grid_factors
  where
    fn (scx,scy) = P2 (dpoint $ scx * fromIntegral x)
                      (dpoint $ scy * fromIntegral y)


-- | Scale a vector coordinate by the snap-grid scaling factors.
--
snapmove :: (Fractional u, PtSize u, DrawingCtxM m) 
       => (Int,Int) -> m (Vec2 u)
snapmove (x,y) = fn <$> query dc_snap_grid_factors
  where
    fn (scx,scy) = V2 (dpoint $ scx * fromIntegral x)
                      (dpoint $ scy * fromIntegral y)



-- | Size of the round corner factor.
--
getRoundCornerSize :: (Fractional u, PtSize u, DrawingCtxM m) => m u
getRoundCornerSize = dpoint <$> query dc_round_corner_factor


-- | Get the (x,y) margin around text.
--
-- Note - not all text operations in Wumpus are drawn with text 
-- margin. 
-- 
getTextMargin :: (Fractional u, PtSize u, DrawingCtxM m) => m (u,u)
getTextMargin = 
    (\(TextMargin xsep ysep) -> (fromPsPoint xsep, fromPsPoint ysep))
      <$> query dc_text_margin



getLineWidth :: DrawingCtxM m => m PsPoint
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



-- | Vertical distance between baselines of consecutive text 
-- lines.
--
baselineSpacing :: (Fractional u, DrawingCtxM m) => m u
baselineSpacing = 
    (\sz factor -> realToFrac $ factor * fromIntegral sz)
      <$> query dc_font_size  <*> query dc_line_spacing_factor

-- | The /mark/ height is the height of a lowercase letter in the 
-- Courier font at the current point size.
--
-- Arrowheads, dots etc. should generally be drawn at the mark 
-- height.
-- 
markHeight :: (PtSize u, DrawingCtxM m) => m u
markHeight = (fromPsPoint . FS.xcharHeight) <$> query dc_font_size


markHalfHeight :: (Fractional u, PtSize u, DrawingCtxM m) => m u
markHalfHeight = (0.5*) <$> markHeight


-- Note - there are probably enough functions that use just 
-- markHeight to merit a withMarkHeight function.


--------------------------------------------------------------------------------

glyphQuery :: DrawingCtxM m => (FontMetrics -> PsPoint -> u) -> m u
glyphQuery fn = (\ctx -> withFontMetrics fn ctx) <$> queryCtx


-- | Get the font bounding box - this is the maximum boundary of 
-- the glyphs in the font. The span of the height is expected to 
-- be bigger than the cap_height plus descender depth.
--
glyphBoundingBox :: (PtSize u, DrawingCtxM m) => m (BoundingBox u)
glyphBoundingBox = glyphQuery get_bounding_box





glyphCapHeight :: (PtSize u, DrawingCtxM m) => m u
glyphCapHeight = glyphQuery get_cap_height

-- | Note - descender is expected to be negative.
--
glyphDescender :: (PtSize u, DrawingCtxM m) => m u
glyphDescender = glyphQuery get_descender

-- | This is the distance from cap_height to descender.
--
glyphVerticalSpan :: (PtSize u, DrawingCtxM m) => m u
glyphVerticalSpan = 
    (\ch dd -> ch - dd) <$> glyphCapHeight <*> glyphDescender


cwLookupTable :: (PtSize u, DrawingCtxM m) => m (CharWidthLookup u)
cwLookupTable = glyphQuery get_cw_table

