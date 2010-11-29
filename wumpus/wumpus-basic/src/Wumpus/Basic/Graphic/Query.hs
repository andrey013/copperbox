{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Query
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Querying the Drawing Context.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Query
  ( 

    
    textAttr
  , withTextAttr

  , strokeAttr
  , withStrokeAttr

  , fillAttr
  , withFillAttr

  , borderedAttr
  , withBorderedAttr

  , getLineWidth
  , getFontAttr
--  , fontSize
--  , fontFace
  , markHeight
  , markHalfHeight
  , baselineSpacing

  -- * Glyph metrics
  , glyphBoundingBox
  , glyphHeightRange
  , glyphHeight
  , glyphCapHeight

  , avLookupTable

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

import Wumpus.Basic.Graphic.Base
import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.GlyphMetrics

import Wumpus.Core                              -- package: wumpus-core
import qualified Wumpus.Core.FontSize   as FS
 
import Control.Applicative


textAttr :: DrawingCtxM m => m (RGBi,FontAttr)
textAttr = (,) <$> asksDC stroke_colour <*> asksDC font_props

-- | Because @textAttr@ is so commonly used here is a functional
-- version that avoids tupling.
--
withTextAttr :: DrawingCtxM m => (RGBi -> FontAttr -> a) -> m a
withTextAttr fn = fn <$> asksDC stroke_colour <*> asksDC font_props


strokeAttr :: DrawingCtxM m => m (RGBi, StrokeAttr)
strokeAttr = (,) <$> asksDC stroke_colour <*> asksDC stroke_props

withStrokeAttr :: DrawingCtxM m => (RGBi -> StrokeAttr -> a) -> m a
withStrokeAttr fn = fn <$> asksDC stroke_colour <*> asksDC stroke_props


fillAttr :: DrawingCtxM m => m RGBi
fillAttr = asksDC fill_colour

withFillAttr :: DrawingCtxM m => (RGBi -> a) -> m a
withFillAttr fn = fn <$> asksDC fill_colour

borderedAttr :: DrawingCtxM m => m (RGBi, StrokeAttr, RGBi)
borderedAttr = (,,) <$> asksDC fill_colour <*> asksDC stroke_props 
                                           <*> asksDC stroke_colour

withBorderedAttr :: DrawingCtxM m => (RGBi -> StrokeAttr -> RGBi -> a) -> m a
withBorderedAttr fn = 
    fn <$> asksDC fill_colour <*> asksDC stroke_props 
                              <*> asksDC stroke_colour




getLineWidth :: DrawingCtxM m => m Double
getLineWidth = line_width <$> asksDC stroke_props

getFontAttr :: DrawingCtxM m => m FontAttr
getFontAttr = asksDC font_props

-- fontSize :: DrawingCtxM m => m Int
-- fontSize = font_size <$> asksDC font_props

-- fontFace :: DrawingCtxM m => m FontFace
-- fontFace = font_face <$> asksDC font_props




-- | Vertical distance between baselines of consecutive text 
-- lines.
--
baselineSpacing :: (DrawingCtxM m, Fractional u) => m u
baselineSpacing = 
    (\sz factor -> realToFrac $ factor * fromIntegral sz)
      <$> asksDC (font_size . font_props) <*> asksDC line_spacing_factor

-- | The /mark/ height is the height of a lowercase letter in the 
-- current font.
--
-- Arrowheads, dots etc. should generally be drawn at the mark 
-- height.
-- 
markHeight :: (DrawingCtxM m, FromPtSize u) => m u
markHeight = (fromPtSize . FS.xcharHeight . font_size) <$> asksDC font_props


markHalfHeight :: (DrawingCtxM m, Fractional u, FromPtSize u) => m u
markHalfHeight = (0.5*) <$> markHeight


-- Note - there are probably enough functions that use just 
-- markHeight to merit a withMarkHeight function.


--------------------------------------------------------------------------------

glyphQuery :: DrawingCtxM m => (GlyphMetrics -> PtSize -> u) -> m u
glyphQuery fn = (\ctx -> withFontMetrics fn ctx) <$> askDC

glyphBoundingBox :: (FromPtSize u, DrawingCtxM m) => m (BoundingBox u)
glyphBoundingBox = glyphQuery get_bounding_box


glyphHeightRange :: (FromPtSize u, DrawingCtxM m) => m (u,u)
glyphHeightRange = fn <$> glyphBoundingBox
  where
    fn (BBox (P2 _ ymin) (P2 _ ymax)) = (ymin,ymax)


glyphHeight :: (FromPtSize u, DrawingCtxM m) => m u
glyphHeight = (\(ymax,ymin) -> ymax - ymin) <$> glyphHeightRange


glyphCapHeight :: (FromPtSize u, DrawingCtxM m) => m u
glyphCapHeight = glyphQuery get_cap_height

avLookupTable :: (FromPtSize u, DrawingCtxM m) => m (Int -> Vec2 u)
avLookupTable = glyphQuery get_av_lookup


--------------------------------------------------------------------------------

withFontSize :: DrawingCtxM m => (FontSize -> u) -> m u
withFontSize fn = (fn . font_size) <$> asksDC font_props


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
      <$> asksDC (font_size . font_props)
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
      <$> monoTotalCharHeight <*> asksDC line_spacing_factor
 
    -- Note as the height calculation has changed in Wumpus-Core this
    -- no longer quite works... 

 

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
