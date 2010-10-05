{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Query
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
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
  , lineSpacing

  -- 
  , monoCharWidth
  , monoSpacerWidth
  , monoTextWidth
  , monoTextLength
  , monoTextHeight
  , monoNumeralHeight
  , monoLowerxHeight
  , monoDescenderDepth
  , monoTextDimensions
  , monoVecToCenter  
  ) where

import Wumpus.Basic.Graphic.BaseTypes
import Wumpus.Basic.Graphic.DrawingContext


import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative


textAttr :: DrawingF (RGBi,FontAttr)
textAttr = (,) <$> asksDF stroke_colour <*> asksDF font_props

-- | Because @textAttr@ is so commonly used here is a functional
-- version that avoids tupling.
--
withTextAttr :: (RGBi -> FontAttr -> a) -> DrawingF a
withTextAttr fn = fn <$> asksDF stroke_colour <*> asksDF font_props


strokeAttr :: DrawingF (RGBi, StrokeAttr)
strokeAttr = (,) <$> asksDF stroke_colour <*> asksDF stroke_props

withStrokeAttr :: (RGBi -> StrokeAttr -> a) -> DrawingF a
withStrokeAttr fn = fn <$> asksDF stroke_colour <*> asksDF stroke_props


fillAttr :: DrawingF RGBi
fillAttr = asksDF fill_colour

withFillAttr :: (RGBi -> a) -> DrawingF a
withFillAttr fn = fn <$> asksDF fill_colour

borderedAttr :: DrawingF (RGBi, StrokeAttr, RGBi)
borderedAttr = (,,) <$> asksDF fill_colour <*> asksDF stroke_props 
                                                <*> asksDF stroke_colour

withBorderedAttr :: (RGBi -> StrokeAttr -> RGBi -> a) -> DrawingF a
withBorderedAttr fn = 
    fn <$> asksDF fill_colour <*> asksDF stroke_props 
                              <*> asksDF stroke_colour




lineWidth :: DrawingF Double
lineWidth = line_width <$> asksDF stroke_props

fontSize :: DrawingF Int
fontSize = font_size <$> asksDF font_props




-- Maybe these functions are better as queries - i.e. functions
-- of type DrawingF, e.g.
-- 
-- > lineSpacing :: Fractional u => DrawingF u
-- 
-- Then the /client/ can just bound the answer directly
-- rather than using 
--
-- > askDF lineSpacing >>= \u -> ...
--

lineSpacing :: Fractional u => DrawingF u
lineSpacing = 
    (\sz factor -> realToFrac $ factor * fromIntegral sz)
      <$> asksDF (font_size . font_props) <*> asksDF line_spacing_factor

-- | The /mark/ height is the height of a lowercase letter in the 
-- current font.
--
-- Arrowheads, dots etc. should generally be drawn at the mark 
-- height.
-- 
markHeight :: FromPtSize u => DrawingF u
markHeight = (fromPtSize . xcharHeight . font_size) <$> asksDF font_props


markHalfHeight :: (Fractional u, FromPtSize u) => DrawingF u
markHalfHeight = (0.5*) <$> markHeight


-- Note - there are probably enough functions that use just 
-- markHeight to merit a withMarkHeight function.






--------------------------------------------------------------------------------

withFontSize :: (FontSize -> u) -> DrawingF u
withFontSize fn = fn . font_size <$> asksDF font_props

monoCharWidth :: FromPtSize u => DrawingF u
monoCharWidth = withFontSize (fromPtSize . charWidth)

monoSpacerWidth :: FromPtSize u => DrawingF u
monoSpacerWidth = withFontSize (fromPtSize . spacerWidth)


monoTextWidth :: FromPtSize u => Int -> DrawingF u
monoTextWidth n = withFontSize $ \sz -> fromPtSize $ textWidth sz n


monoTextLength :: FromPtSize u => String -> DrawingF u
monoTextLength ss = monoTextWidth $ charCount ss


monoTextHeight :: FromPtSize u => DrawingF u
monoTextHeight = withFontSize (fromPtSize . textHeight)

monoNumeralHeight :: FromPtSize u => DrawingF u
monoNumeralHeight = withFontSize (fromPtSize . numeralHeight)


-- | Height of a lower case \'x\' in Courier.
--  
-- \'x\' has no ascenders or descenders. 
-- 
monoLowerxHeight :: FromPtSize u => DrawingF u
monoLowerxHeight = withFontSize (fromPtSize . xcharHeight)

monoDescenderDepth :: FromPtSize u => DrawingF u
monoDescenderDepth = withFontSize (fromPtSize . descenderDepth)


-- | Query the dimensions of the text using the current font size
-- but using metrics derived from Courier.
--
-- Note - the width will generally be a over-estimate for 
-- non-monospaced fonts.
-- 
monoTextDimensions :: (Num u, Ord u, FromPtSize u) => String -> DrawingF (u,u)
monoTextDimensions ss = 
    (\sz -> post $ textBounds sz zeroPt ss) 
      <$> asksDF (font_size . font_props)
  where
    post bb = (boundaryWidth bb, boundaryHeight bb)


-- | Vector from baseline left to center
monoVecToCenter :: (Fractional u, Ord u, FromPtSize u) 
                => String -> DrawingF (Vec2 u)
monoVecToCenter ss = (\(w,h) dy -> vec (0.5*w) (0.5*h - dy)) 
                       <$> monoTextDimensions ss <*> monoDescenderDepth
