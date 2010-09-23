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
  , textDimensions
  , markHeight
  , lineSpacing
  , monoLowerxHeight
  , monoDescenderDepth
  
  ) where

import Wumpus.Basic.Graphic.BaseTypes
import Wumpus.Basic.Graphic.DrawingContext


import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative


textAttr :: DrawingF (RGBi,FontAttr)
textAttr = (,) <$> asksDF primary_colour <*> asksDF font_props

-- | Because @textAttr@ is so commonly used here is a functional
-- version that avoids tupling.
--
withTextAttr :: (RGBi -> FontAttr -> a) -> DrawingF a
withTextAttr fn = fn <$> asksDF primary_colour <*> asksDF font_props


strokeAttr :: DrawingF (RGBi, StrokeAttr)
strokeAttr = (,) <$> asksDF primary_colour <*> asksDF stroke_props

withStrokeAttr :: (RGBi -> StrokeAttr -> a) -> DrawingF a
withStrokeAttr fn = fn <$> asksDF primary_colour <*> asksDF stroke_props


fillAttr :: DrawingF RGBi
fillAttr = asksDF secondary_colour

withFillAttr :: (RGBi -> a) -> DrawingF a
withFillAttr fn = fn <$> asksDF secondary_colour

borderedAttr :: DrawingF (RGBi, StrokeAttr, RGBi)
borderedAttr = (,,) <$> asksDF secondary_colour <*> asksDF stroke_props 
                                                <*> asksDF primary_colour

withBorderedAttr :: (RGBi -> StrokeAttr -> RGBi -> a) -> DrawingF a
withBorderedAttr fn = 
    fn <$> asksDF secondary_colour <*> asksDF stroke_props 
                                   <*> asksDF primary_colour




lineWidth :: DrawingF Double
lineWidth = line_width <$> asksDF stroke_props

fontSize :: DrawingF Int
fontSize = font_size <$> asksDF font_props


-- | Query the dimensions of the text using the current font size
-- and metrics derived from Courier.
--
-- Note - the width will generally be a over-estimate for 
-- non-monospaced fonts.
-- 
textDimensions :: (Num u, Ord u, FromPtSize u) => String -> DrawingF (u,u)
textDimensions ss = 
    (\sz -> post $ textBounds sz zeroPt ss) 
      <$> asksDF (font_size . font_props)
  where
    post bb = (boundaryWidth bb, boundaryHeight bb)


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

-- | A Mark is consider to be the height of a lowercase letter
-- in the current font.
-- 
-- Note better to use xlowerHeight
-- 
markHeight :: FromPtSize u => DrawingF u
markHeight = (fromPtSize . xcharHeight . font_size) <$> asksDF font_props

-- Note - there are probably enough functions that use just 
-- markHeight to merit a withMarkHeight function.


-- | Height of a lower case \'x\' in Courier.
--  
-- \'x\' has no ascenders or descenders. 
-- 
monoLowerxHeight :: FromPtSize u => DrawingF u
monoLowerxHeight = fromPtSize . xcharHeight . font_size <$> asksDF font_props

monoDescenderDepth :: FromPtSize u => DrawingF u
monoDescenderDepth = 
    fromPtSize . descenderDepth . font_size <$> asksDF font_props
