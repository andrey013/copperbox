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

import Wumpus.Basic.Graphic.Base
import Wumpus.Basic.Graphic.DrawingContext


import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative


textAttr :: (Applicative m, DrawingCtxM m) => m (RGBi,FontAttr)
textAttr = (,) <$> asksDC stroke_colour <*> asksDC font_props

-- | Because @textAttr@ is so commonly used here is a functional
-- version that avoids tupling.
--
withTextAttr :: (Applicative m, DrawingCtxM m) 
             => (RGBi -> FontAttr -> a) -> m a
withTextAttr fn = fn <$> asksDC stroke_colour <*> asksDC font_props


strokeAttr :: (Applicative m, DrawingCtxM m) => m (RGBi, StrokeAttr)
strokeAttr = (,) <$> asksDC stroke_colour <*> asksDC stroke_props

withStrokeAttr :: (Applicative m, DrawingCtxM m) 
               => (RGBi -> StrokeAttr -> a) -> m a
withStrokeAttr fn = fn <$> asksDC stroke_colour <*> asksDC stroke_props


fillAttr :: (Applicative m, DrawingCtxM m) => m RGBi
fillAttr = asksDC fill_colour

withFillAttr :: (Applicative m, DrawingCtxM m) => (RGBi -> a) -> m a
withFillAttr fn = fn <$> asksDC fill_colour

borderedAttr :: (Applicative m, DrawingCtxM m) => m (RGBi, StrokeAttr, RGBi)
borderedAttr = (,,) <$> asksDC fill_colour <*> asksDC stroke_props 
                                           <*> asksDC stroke_colour

withBorderedAttr :: (Applicative m, DrawingCtxM m) 
                 => (RGBi -> StrokeAttr -> RGBi -> a) -> m a
withBorderedAttr fn = 
    fn <$> asksDC fill_colour <*> asksDC stroke_props 
                              <*> asksDC stroke_colour




lineWidth :: (Applicative m, DrawingCtxM m) => m Double
lineWidth = line_width <$> asksDC stroke_props

fontSize :: (Applicative m, DrawingCtxM m) => m Int
fontSize = font_size <$> asksDC font_props




-- Maybe these functions are better as queries - i.e. functions
-- of type DrawingR, e.g.
-- 
-- > lineSpacing :: Fractional u => DrawingR u
-- 
-- Then the /client/ can just bound the answer directly
-- rather than using 
--
-- > askDF lineSpacing >>= \u -> ...
--

lineSpacing :: (Applicative m, DrawingCtxM m, Fractional u) => m u
lineSpacing = 
    (\sz factor -> realToFrac $ factor * fromIntegral sz)
      <$> asksDC (font_size . font_props) <*> asksDC line_spacing_factor

-- | The /mark/ height is the height of a lowercase letter in the 
-- current font.
--
-- Arrowheads, dots etc. should generally be drawn at the mark 
-- height.
-- 
markHeight :: (Applicative m, DrawingCtxM m, FromPtSize u) => m u
markHeight = (fromPtSize . xcharHeight . font_size) <$> asksDC font_props


markHalfHeight :: (Applicative m, DrawingCtxM m, Fractional u, FromPtSize u) 
               => m u
markHalfHeight = (0.5*) <$> markHeight


-- Note - there are probably enough functions that use just 
-- markHeight to merit a withMarkHeight function.






--------------------------------------------------------------------------------

withFontSize :: (Applicative m, DrawingCtxM m) => (FontSize -> u) -> m u
withFontSize fn = (fn . font_size) <$> asksDC font_props

monoCharWidth :: (Applicative m, DrawingCtxM m, FromPtSize u) => m u
monoCharWidth = withFontSize (fromPtSize . charWidth)

monoSpacerWidth :: (Applicative m, DrawingCtxM m, FromPtSize u) => m u
monoSpacerWidth = withFontSize (fromPtSize . spacerWidth)


monoTextWidth :: (Applicative m, DrawingCtxM m, FromPtSize u) => Int -> m u
monoTextWidth n = withFontSize $ \sz -> fromPtSize $ textWidth sz n


monoTextLength :: (Applicative m, DrawingCtxM m, FromPtSize u) => String -> m u
monoTextLength ss = monoTextWidth $ charCount ss


monoTextHeight :: (Applicative m, DrawingCtxM m, FromPtSize u) => m u
monoTextHeight = withFontSize (fromPtSize . textHeight)

monoNumeralHeight :: (Applicative m, DrawingCtxM m, FromPtSize u) => m u
monoNumeralHeight = withFontSize (fromPtSize . numeralHeight)


-- | Height of a lower case \'x\' in Courier.
--  
-- \'x\' has no ascenders or descenders. 
-- 
monoLowerxHeight :: (Applicative m, DrawingCtxM m, FromPtSize u) => m u
monoLowerxHeight = withFontSize (fromPtSize . xcharHeight)

monoDescenderDepth :: (Applicative m, DrawingCtxM m, FromPtSize u) => m u
monoDescenderDepth = withFontSize (fromPtSize . descenderDepth)


-- | Query the dimensions of the text using the current font size
-- but using metrics derived from Courier.
--
-- Note - the width will generally be a over-estimate for 
-- non-monospaced fonts.
-- 
monoTextDimensions :: (Applicative m, DrawingCtxM m, Num u, Ord u, FromPtSize u)
                   => String -> m (u,u)
monoTextDimensions ss = 
    (\sz -> post $ textBounds sz zeroPt ss) 
      <$> asksDC (font_size . font_props)
  where
    post bb = (boundaryWidth bb, boundaryHeight bb)


-- | Vector from baseline left to center
monoVecToCenter :: ( Applicative m, DrawingCtxM m
                   , Fractional u, Ord u, FromPtSize u ) 
                => String -> m (Vec2 u)
monoVecToCenter ss = (\(w,h) dy -> vec (0.5*w) (0.5*h - dy)) 
                       <$> monoTextDimensions ss <*> monoDescenderDepth
