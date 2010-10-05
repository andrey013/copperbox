{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.DrawingContext
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
  , DrawingContextF

  , standardContext
  , default_drawing_context

  -- * Modifiers 
  -- ** Line widths
  , thick
  , ultrathick
  , thin

  , capButt
  , capRound
  , capSquare

  , joinMiter
  , joinRound
  , joinBevel

  -- ** Dash Pattern
  , dashPattern
  , unit_dash_pattern
  , phase
  , dphase
  , doublegaps
  , doubledashes

  -- ** Font properties
  , fontsize
  , fontface

  -- ** Mark drawing size
  , doublesize
  , halfsize

  -- ** Colour
  , swapColours
  , bothStrokeColour
  , bothFillColour
  , strokeColour
  , fillColour 



  
  ) where


import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Colour.SVGColours

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative

data DrawingContext = DrawingContext
      { stroke_props          :: StrokeAttr
      , font_props            :: FontAttr
      , stroke_colour         :: RGBi      -- also text colour...
      , fill_colour           :: RGBi      
      , line_spacing_factor   :: Double
      }
  deriving (Eq,Show)

type DrawingContextF = DrawingContext -> DrawingContext


standardContext :: FontSize -> DrawingContext
standardContext sz = 
    DrawingContext { stroke_props         = default_stroke_attr
                   , font_props           = FontAttr sz courier
                   , stroke_colour        = black
                   , fill_colour          = light_gray
                   , line_spacing_factor  = 1.2  
                   }


default_drawing_context :: DrawingContext
default_drawing_context = 
    standardContext (font_size wumpus_default_font)


updateStrokeProps :: (StrokeAttr -> StrokeAttr) -> DrawingContextF
updateStrokeProps fn = (\s i -> s { stroke_props = fn i }) <*> stroke_props

updateFontProps :: (FontAttr -> FontAttr) -> DrawingContextF
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

setLineWidth       :: Double -> DrawingContextF
setLineWidth d      = updateStrokeProps (\s -> s { line_width = d })

-- | Set the line width to a /thick/.
--
-- Note this context update is /oblivious/ - operationally the 
-- line width is set to exactly @2.0@.
--
thick               :: DrawingContextF
thick               = setLineWidth thick_line

ultrathick          :: DrawingContextF
ultrathick          = setLineWidth ultra_thick_line

thin                :: DrawingContextF
thin                = setLineWidth thin_line


setLineCap          :: LineCap -> DrawingContextF
setLineCap d        = updateStrokeProps (\s -> s { line_cap = d })


capButt             :: DrawingContextF
capButt             = setLineCap CapButt

capRound            :: DrawingContextF
capRound            = setLineCap CapRound

capSquare           :: DrawingContextF
capSquare           = setLineCap CapSquare


setLineJoin         :: LineJoin -> DrawingContextF
setLineJoin d       = updateStrokeProps (\s -> s { line_join = d })


joinMiter           :: DrawingContextF
joinMiter           = setLineJoin JoinMiter

joinRound           :: DrawingContextF
joinRound           = setLineJoin JoinRound

joinBevel           :: DrawingContextF
joinBevel           = setLineJoin JoinBevel


--------------------------------------------------------------------------------

dashPattern         :: DashPattern -> DrawingContextF
dashPattern d       = updateStrokeProps (\s -> s { dash_pattern = d })        

unit_dash_pattern   :: DashPattern
unit_dash_pattern   = Dash 0 [(1,1)]

-- oblivious
phase               :: Int -> DashPattern -> DashPattern
phase _ Solid       = Solid
phase i (Dash _ xs) = Dash i xs

-- non-oblivious
dphase               :: Int -> DashPattern -> DashPattern
dphase _ Solid       = Solid
dphase d (Dash i xs) = Dash (i+d) xs

doublegaps              :: DashPattern -> DashPattern
doublegaps Solid        = Solid
doublegaps (Dash i xs)  = Dash i (map fn xs)
  where
    fn (a,b) = (a,2*b)

doubledashes              :: DashPattern -> DashPattern
doubledashes Solid        = Solid
doubledashes (Dash i xs)  = Dash i (map fn xs)
  where
    fn (a,b) = (a*2,b)


--------------------------------------------------------------------------------


fontface            :: FontFace -> DrawingContextF
fontface ff         = updateFontProps (\(FontAttr sz _) -> FontAttr sz ff)

fontsize            :: Int -> DrawingContextF
fontsize sz         = updateFontProps (\(FontAttr _ ff) -> FontAttr sz ff)

--------------------------------------------------------------------------------

-- | Set the font size to double the current size, note the font
-- size also controls the size of dots, arrowsheads etc.
-- 
doublesize          :: DrawingContextF
doublesize          = (\s sz -> fontsize (sz*2) s) <*> (font_size . font_props)


-- | Set the font size to half the current size, note the font
-- size also controls the size of dots, arrowsheads etc.
-- 
-- As fontsize is an integer this is not exact - half size of
-- 15pt type is 7pt.
-- 
halfsize            :: DrawingContextF
halfsize            = (\s sz -> fontsize (sz `div` 2) s) 
                        <*> (font_size . font_props)


--------------------------------------------------------------------------------

swapColours :: DrawingContextF
swapColours = 
    (\s a b -> s { stroke_colour = b, fill_colour = a })
        <*> stroke_colour <*> fill_colour

bothStrokeColour :: DrawingContextF
bothStrokeColour = (\s a -> s { fill_colour = a }) <*> stroke_colour

bothFillColour :: DrawingContextF
bothFillColour = (\s a -> s { stroke_colour = a }) <*> fill_colour



strokeColour :: RGBi -> DrawingContextF
strokeColour rgb = \s -> s { stroke_colour = rgb } 


fillColour :: RGBi -> DrawingContextF
fillColour rgb = \s -> s { fill_colour = rgb } 

