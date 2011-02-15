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

  -- * Default monospace metrics

  , monoFontPointSize
  , monoCharWidth
  , monoTextWidth
  , monoTextLength
  , monoCapHeight
  , monoLowerxHeight
  , monoDescenderDepth
  , monoAscenderHeight
  , monoTextDimensions
  , monoMultiLineHeight
  , monoDefaultPadding
  , monoVecToCenter  
  ) where

import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.FontMetrics

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
position :: (Fractional u, DrawingCtxM m) 
         => (Int,Int) -> m (Point2 u)
position (x,y) = fn <$> query dc_snap_grid_factors
  where
    fn (scx,scy) = P2 ((realToFrac scx) * fromIntegral x)
                      ((realToFrac scy) * fromIntegral y)



-- | Scale a vector coordinate by the snap-grid scaling factors.
--
snapmove :: (Fractional u, DrawingCtxM m) 
       => (Int,Int) -> m (Vec2 u)
snapmove (x,y) = fn <$> query dc_snap_grid_factors
  where
    fn (scx,scy) = V2 ((realToFrac scx) * fromIntegral x)
                      ((realToFrac scy) * fromIntegral y)



-- | Size of the round corner factor.
--
getRoundCornerSize :: (DrawingCtxM m, Fractional u, FromPtSize u) => m u
getRoundCornerSize = (\factor -> (realToFrac factor) * fromPtSize 1)
                        <$> query dc_round_corner_factor



-- | Get the (x,y) margin around text.
--
-- Note - not all text operations in Wumpus are drawn with text 
-- margin. 
-- 
getTextMargin :: (DrawingCtxM m, Fractional u, FromPtSize u) => m (u,u)
getTextMargin = (\(TextMargin xsep ysep) -> (fn xsep, fn ysep))
                    <$> query dc_text_margin
  where
    fn d = (realToFrac d) * fromPtSize 1



getLineWidth :: DrawingCtxM m => m Pt
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
baselineSpacing :: (DrawingCtxM m, Fractional u) => m u
baselineSpacing = 
    (\sz factor -> realToFrac $ factor * fromIntegral sz)
      <$> query dc_font_size  <*> query dc_line_spacing_factor

-- | The /mark/ height is the height of a lowercase letter in the 
-- Courier font at the current point size.
--
-- Arrowheads, dots etc. should generally be drawn at the mark 
-- height.
-- 
markHeight :: (DrawingCtxM m, FromPtSize u) => m u
markHeight = (fromPtSize . FS.xcharHeight) <$> query dc_font_size


markHalfHeight :: (DrawingCtxM m, Fractional u, FromPtSize u) => m u
markHalfHeight = (0.5*) <$> markHeight


-- Note - there are probably enough functions that use just 
-- markHeight to merit a withMarkHeight function.


--------------------------------------------------------------------------------

glyphQuery :: DrawingCtxM m => (FontMetrics -> PtSize -> u) -> m u
glyphQuery fn = (\ctx -> withFontMetrics fn ctx) <$> queryCtx

-- | Get the font bounding box - this is the maximum boundary of 
-- the glyphs in the font. The span of the height is expected to 
-- be bigger than the cap_height plus descender depth.
--
glyphBoundingBox :: (FromPtSize u, DrawingCtxM m) => m (BoundingBox u)
glyphBoundingBox = glyphQuery get_bounding_box





glyphCapHeight :: (FromPtSize u, DrawingCtxM m) => m u
glyphCapHeight = glyphQuery get_cap_height

-- | Note - descender is expected to be negative.
--
glyphDescender :: (FromPtSize u, DrawingCtxM m) => m u
glyphDescender = glyphQuery get_descender

-- | This is the distance from cap_height to descender.
--
glyphVerticalSpan :: (FromPtSize u, DrawingCtxM m) => m u
glyphVerticalSpan = 
    (\ch dd -> ch - dd) <$> glyphCapHeight <*> glyphDescender


cwLookupTable :: (FromPtSize u, DrawingCtxM m) => m (CharWidthLookup u)
cwLookupTable = glyphQuery get_cw_table


--------------------------------------------------------------------------------

withFontSize :: DrawingCtxM m => (FontSize -> u) -> m u
withFontSize fn = fn <$> query dc_font_size


-- NOTE - textHeight in Wumpus-Core should be renamed as it is
-- (probably) more indiactive of Cap height than /font point size/
--

monoFontPointSize :: (DrawingCtxM m, FromPtSize u) => m u
monoFontPointSize = withFontSize (fromPtSize . fromIntegral)

monoCharWidth :: (DrawingCtxM m, FromPtSize u) => m u
monoCharWidth = withFontSize (fromPtSize . FS.charWidth)


monoTextWidth :: (DrawingCtxM m, FromPtSize u) => Int -> m u
monoTextWidth n = withFontSize $ \sz -> fromPtSize $ FS.textWidth sz n


monoTextLength :: (DrawingCtxM m, FromPtSize u) => String -> m u
monoTextLength ss = monoTextWidth $ charCount ss


monoCapHeight :: (DrawingCtxM m, FromPtSize u) => m u
monoCapHeight = withFontSize (fromPtSize . FS.capHeight)

monoTotalCharHeight :: (DrawingCtxM m, FromPtSize u) => m u
monoTotalCharHeight = withFontSize (fromPtSize . FS.totalCharHeight)



-- | Height of a lower case \'x\' in Courier.
--  
-- \'x\' has no ascenders or descenders. 
-- 
monoLowerxHeight :: (DrawingCtxM m, FromPtSize u) => m u
monoLowerxHeight = withFontSize (fromPtSize . FS.xcharHeight)

monoDescenderDepth :: (DrawingCtxM m, FromPtSize u) => m u
monoDescenderDepth = withFontSize (fromPtSize . FS.descenderDepth)

monoAscenderHeight :: (DrawingCtxM m, FromPtSize u) => m u
monoAscenderHeight = withFontSize (fromPtSize . FS.ascenderHeight)


-- | Query the dimensions of the text using the current font size
-- but using metrics derived from Courier.
--
-- Note - the width will generally be a over-estimate for 
-- non-monospaced fonts.
-- 
monoTextDimensions :: (DrawingCtxM m, Num u, Ord u, FromPtSize u)
                   => String -> m (u,u)
monoTextDimensions ss = 
    (\sz -> post $ textBounds sz zeroPt ss) 
      <$> query dc_font_size
  where
    post bb = (boundaryWidth bb, boundaryHeight bb)


-- | The heigth of @n@ lines of text, which is 
-- @n lines + n-1 line spacers@
--
monoMultiLineHeight :: (DrawingCtxM m, Fractional u, FromPtSize u) 
                        => Int -> m u
monoMultiLineHeight n | n < 0   = pure 0
monoMultiLineHeight n           = 
    (\h lsf -> h + (fromIntegral $ n-1) * (h * realToFrac lsf))
      <$> monoTotalCharHeight <*> query dc_line_spacing_factor
 
    -- Note as the height calculation has changed in Wumpus-Core this
    -- no longer quite works... 

 
{-# DEPRECATED monoDefaultPadding "Needs a rethink" #-}

-- | The default padding is half of the /char width/.
--
monoDefaultPadding :: (DrawingCtxM m, Fractional u, FromPtSize u) => m u
monoDefaultPadding = (0.5*) <$> monoCharWidth



-- | Vector from baseline left to center
--
monoVecToCenter :: (DrawingCtxM m, Fractional u, Ord u, FromPtSize u) 
                => String -> m (Vec2 u)
monoVecToCenter ss = (\(w,h) dy -> vec (0.5*w) (0.5*h - dy)) 
                       <$> monoTextDimensions ss <*> monoDescenderDepth
