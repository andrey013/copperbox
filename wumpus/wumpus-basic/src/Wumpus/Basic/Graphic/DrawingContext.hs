{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.DrawingAttr
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Drawing attributes
--
-- \*\* WARNING \*\* - this module needs systematic naming 
-- schemes both for update functions (primaryColour, ...) and 
-- for synthesized selectors (e.g. lowerxHeight). The current 
-- names will change.
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.DrawingContext
  ( 

  -- * Drawing context
    DrawingContext(..)

  , standardContext
  , textAttr
  , markHeight
  , lineSpacing
  , lowerxHeight
  , textDimensions

  -- * Modifiers 
  -- ** Line widths
  , thick
  , ultrathick
  , thin

  -- ** Dash Pattern
  , dashPattern

  -- ** Font properties
  , fontsize
  , fontface

  -- ** Colour
  , swapColours
  , primaryColour
  , secondaryColour 



  
  ) where


import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Colour.SVGColours

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative

data DrawingContext = DrawingContext
      { stroke_props          :: StrokeAttr
      , font_props            :: FontAttr
      , primary_colour        :: RGBi      -- usually the stroke colour
      , secondary_colour      :: RGBi      -- usually the fill colour
      , line_spacing_factor   :: Double
      }
  deriving (Eq,Show)


standardContext :: FontSize -> DrawingContext
standardContext sz = 
    DrawingContext { stroke_props           = default_stroke_attr
                   , font_props             = FontAttr sz courier
                   , primary_colour         = black
                   , secondary_colour       = light_gray
                   , line_spacing_factor    = 1.2  
                   }



textAttr :: DrawingContext -> (RGBi,FontAttr)
textAttr = liftA2 (,) primary_colour font_props

lineSpacing :: Fractional u => DrawingContext -> u
lineSpacing = (\sz factor -> realToFrac $ factor * fromIntegral sz)
                <$> (font_size . font_props) <*> line_spacing_factor

-- | A Mark is consider to be the height of a lowercase letter
-- in the current font.
-- 
-- Note better to use xlowerHeight
-- 
markHeight :: FromPtSize u => DrawingContext -> u
markHeight = fromPtSize . xcharHeight . font_size . font_props


-- | Height of a lower case \'x\' in Courier.
--  
-- \'x\' has no ascenders or descenders. 
-- 
lowerxHeight :: FromPtSize u => DrawingContext -> u
lowerxHeight = fromPtSize . xcharHeight . font_size . font_props


-- | textDimensions : text -> DrawingContext -> (width,height)
--
textDimensions :: FromPtSize u => String -> DrawingContext -> (u,u)
textDimensions str attr = (w,h)
  where
    sz = font_size  $ font_props attr
    w  = fromPtSize $ textWidth  sz (charCount str)
    h  = fromPtSize $ numeralHeight sz



updateStrokeProps :: (StrokeAttr -> StrokeAttr) 
                  -> DrawingContext -> DrawingContext
updateStrokeProps fn = (\s i -> s { stroke_props = fn i }) <*> stroke_props

updateFontProps :: (FontAttr -> FontAttr) 
                -> DrawingContext -> DrawingContext
updateFontProps fn = (\s i -> s { font_props = fn i }) <*> font_props


--------------------------------------------------------------------------------
-- line widths

-- Note - some care might be needed if we ever define other unit 
-- types...

-- std_line_width      :: Double
-- std_line_width      = 1.0

thick_line          :: Double
thick_line          = 2.0

ultra_thick_line    :: Double
ultra_thick_line    = 4.0

thin_line           :: Double
thin_line           = 0.5

setLineWidth       :: Double -> DrawingContext -> DrawingContext
setLineWidth d      = updateStrokeProps (\s -> s { line_width = d })


thick               :: DrawingContext -> DrawingContext
thick               = setLineWidth thick_line

ultrathick          :: DrawingContext -> DrawingContext
ultrathick          = setLineWidth ultra_thick_line

thin                :: DrawingContext -> DrawingContext
thin                = setLineWidth thin_line


dashPattern         :: DashPattern -> DrawingContext -> DrawingContext
dashPattern d       = updateStrokeProps (\s -> s { dash_pattern = d })        

--------------------------------------------------------------------------------


fontface            :: FontFace -> DrawingContext -> DrawingContext
fontface ff         = updateFontProps (\(FontAttr sz _) -> FontAttr sz ff)

fontsize            :: Int -> DrawingContext -> DrawingContext
fontsize sz         = updateFontProps (\(FontAttr _ ff) -> FontAttr sz ff)

--------------------------------------------------------------------------------

swapColours :: DrawingContext -> DrawingContext
swapColours = 
    (\s a b -> s { primary_colour = b, secondary_colour = a })
        <*> primary_colour <*> secondary_colour

primaryColour :: RGBi -> DrawingContext -> DrawingContext
primaryColour rgb = \s -> s { primary_colour = rgb } 


secondaryColour :: RGBi -> DrawingContext -> DrawingContext
secondaryColour rgb = \s -> s { secondary_colour = rgb } 

