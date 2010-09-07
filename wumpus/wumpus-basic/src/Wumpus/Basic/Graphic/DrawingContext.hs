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
-- Note - this module will replace @Basic.Graphic.DrawingAttr@ 
-- in due course.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.DrawingContext
  ( 

  -- * Drawing context
    DrawingContext(..)

  , standardContext
  , textAttr
  , markHeight
  , textDimensions

  -- * Modifiers 
  -- ** Line widths
  , thick
  , ultrathick
  , thin

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
import Wumpus.Basic.Utils.Combinators

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative

data DrawingContext = DrawingContext
      { stroke_props       :: StrokeAttr
      , font_props         :: FontAttr
      , primary_colour     :: RGBi      -- usually the stroke colour
      , secondary_colour   :: RGBi      -- usually the fill colour
      }
  deriving (Eq,Show)

standardContext :: FontSize -> DrawingContext
standardContext sz = DrawingContext { stroke_props      = default_stroke_attr
                                    , font_props        = FontAttr sz courier
                                    , primary_colour    = black
                                    , secondary_colour  = light_gray  }



textAttr :: DrawingContext -> (RGBi,FontAttr)
textAttr = liftA2 (,) primary_colour font_props


-- | A Mark is consider to be the height of a lowercase letter
-- in the current font.
--
markHeight :: FromPtSize u => DrawingContext -> u
markHeight = fromPtSize . xcharHeight . font_size . font_props


-- | textDimensions : text -> DrawingContext -> (width,height)
--
textDimensions :: FromPtSize u => String -> DrawingContext -> (u,u)
textDimensions str attr = (w,h)
  where
    sz = font_size  $ font_props attr
    w  = fromPtSize $ textWidth  sz (1 + length str) 
    h  = fromPtSize $ textHeight sz



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
setLineWidth d      = star (\s i -> s { stroke_props = upd i} ) stroke_props
  where
   upd attrs        = attrs { line_width = d }


thick               :: DrawingContext -> DrawingContext
thick               = setLineWidth thick_line

ultrathick          :: DrawingContext -> DrawingContext
ultrathick          = setLineWidth ultra_thick_line

thin                :: DrawingContext -> DrawingContext
thin                = setLineWidth thin_line


fontface            :: FontFace -> DrawingContext -> DrawingContext
fontface ff         = star (\s i -> s { font_props = upd i }) font_props
  where
    upd (FontAttr sz _) = FontAttr sz ff

fontsize            :: Int -> DrawingContext -> DrawingContext
fontsize sz         = star (\s i -> s { font_props = upd i }) font_props
  where
    upd (FontAttr _ ff) = FontAttr sz ff

--------------------------------------------------------------------------------

swapColours :: DrawingContext -> DrawingContext
swapColours = 
    star2 (\s a b -> s { primary_colour = b, secondary_colour = a })
          primary_colour
          secondary_colour

primaryColour :: RGBi -> DrawingContext -> DrawingContext
primaryColour rgb = \s -> s { primary_colour = rgb } 


secondaryColour :: RGBi -> DrawingContext -> DrawingContext
secondaryColour rgb = \s -> s { secondary_colour = rgb } 

