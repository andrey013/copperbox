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

  , lineWidth
  , fontSize
  , markHeight
  , markHalfHeight
  , baselineSpacing

  -- 

  , monoFontPointSize
  , monoCharWidth
  , monoSpacerWidth
  , monoTextWidth
  , monoTextLength
  , monoTextHeight
  , monoNumeralHeight
  , monoLowerxHeight
  , monoDescenderDepth
  , monoTextDimensions
  , monoMultiLineTextHeight
  , monoDefaultPadding
  , monoVecToCenter  
  ) where

import Wumpus.Basic.Graphic.Base
import Wumpus.Basic.Graphic.DrawingContext


import Wumpus.Core                      -- package: wumpus-core

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




lineWidth :: DrawingCtxM m => m Double
lineWidth = line_width <$> asksDC stroke_props

fontSize :: DrawingCtxM m => m Int
fontSize = font_size <$> asksDC font_props





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
markHeight = (fromPtSize . xcharHeight . font_size) <$> asksDC font_props


markHalfHeight :: (DrawingCtxM m, Fractional u, FromPtSize u) => m u
markHalfHeight = (0.5*) <$> markHeight


-- Note - there are probably enough functions that use just 
-- markHeight to merit a withMarkHeight function.






--------------------------------------------------------------------------------

withFontSize :: DrawingCtxM m => (FontSize -> u) -> m u
withFontSize fn = (fn . font_size) <$> asksDC font_props


-- NOTE - textHeight in Wumpus-Core should be renamed as it is
-- (probably) more indiactive of Cap height than /font point size/
--

monoFontPointSize :: (DrawingCtxM m, FromPtSize u) => m u
monoFontPointSize = withFontSize (fromPtSize . textHeight)

monoCharWidth :: (DrawingCtxM m, FromPtSize u) => m u
monoCharWidth = withFontSize (fromPtSize . charWidth)

monoSpacerWidth :: (DrawingCtxM m, FromPtSize u) => m u
monoSpacerWidth = withFontSize (fromPtSize . spacerWidth)


monoTextWidth :: (DrawingCtxM m, FromPtSize u) => Int -> m u
monoTextWidth n = withFontSize $ \sz -> fromPtSize $ textWidth sz n


monoTextLength :: (DrawingCtxM m, FromPtSize u) => String -> m u
monoTextLength ss = monoTextWidth $ charCount ss


monoTextHeight :: (DrawingCtxM m, FromPtSize u) => m u
monoTextHeight = withFontSize (fromPtSize . textHeight)

monoNumeralHeight :: (DrawingCtxM m, FromPtSize u) => m u
monoNumeralHeight = withFontSize (fromPtSize . numeralHeight)


-- | Height of a lower case \'x\' in Courier.
--  
-- \'x\' has no ascenders or descenders. 
-- 
monoLowerxHeight :: (DrawingCtxM m, FromPtSize u) => m u
monoLowerxHeight = withFontSize (fromPtSize . xcharHeight)

monoDescenderDepth :: (DrawingCtxM m, FromPtSize u) => m u
monoDescenderDepth = withFontSize (fromPtSize . descenderDepth)


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
monoMultiLineTextHeight :: (DrawingCtxM m, Fractional u, FromPtSize u) 
                        => Int -> m u
monoMultiLineTextHeight n | n < 0   = pure 0
monoMultiLineTextHeight n           = 
    (\h lsf -> h + (fromIntegral $ n-1) * (h * realToFrac lsf))
      <$> monoTextHeight <*> asksDC line_spacing_factor
 

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
