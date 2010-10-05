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

  , standardContext

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
  , bothPrimary
  , bothSecondary
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

-- | Set the line width to a /thick/.
--
-- Note this context update is /oblivious/ - operationally the 
-- line width is set to exactly @2.0@.
--
thick               :: DrawingContext -> DrawingContext
thick               = setLineWidth thick_line

ultrathick          :: DrawingContext -> DrawingContext
ultrathick          = setLineWidth ultra_thick_line

thin                :: DrawingContext -> DrawingContext
thin                = setLineWidth thin_line


setLineCap          :: LineCap -> DrawingContext -> DrawingContext
setLineCap d        = updateStrokeProps (\s -> s { line_cap = d })


capButt             :: DrawingContext -> DrawingContext
capButt             = setLineCap CapButt

capRound            :: DrawingContext -> DrawingContext
capRound            = setLineCap CapRound

capSquare           :: DrawingContext -> DrawingContext
capSquare           = setLineCap CapSquare


setLineJoin         :: LineJoin -> DrawingContext -> DrawingContext
setLineJoin d       = updateStrokeProps (\s -> s { line_join = d })


joinMiter           :: DrawingContext -> DrawingContext
joinMiter           = setLineJoin JoinMiter

joinRound           :: DrawingContext -> DrawingContext
joinRound           = setLineJoin JoinRound

joinBevel           :: DrawingContext -> DrawingContext
joinBevel           = setLineJoin JoinBevel


--------------------------------------------------------------------------------

dashPattern         :: DashPattern -> DrawingContext -> DrawingContext
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


fontface            :: FontFace -> DrawingContext -> DrawingContext
fontface ff         = updateFontProps (\(FontAttr sz _) -> FontAttr sz ff)

fontsize            :: Int -> DrawingContext -> DrawingContext
fontsize sz         = updateFontProps (\(FontAttr _ ff) -> FontAttr sz ff)

--------------------------------------------------------------------------------

-- | Set the font size to double the current size, note the font
-- size also controls the size of dots, arrowsheads etc.
-- 
doublesize          :: DrawingContext -> DrawingContext
doublesize          = (\s sz -> fontsize (sz*2) s) <*> (font_size . font_props)


-- | Set the font size to half the current size, note the font
-- size also controls the size of dots, arrowsheads etc.
-- 
-- As fontsize is an integer this is not exact - half size of
-- 15pt type is 7pt.
-- 
halfsize            :: DrawingContext -> DrawingContext
halfsize            = (\s sz -> fontsize (sz `div` 2) s) 
                        <*> (font_size . font_props)


--------------------------------------------------------------------------------

swapColours :: DrawingContext -> DrawingContext
swapColours = 
    (\s a b -> s { primary_colour = b, secondary_colour = a })
        <*> primary_colour <*> secondary_colour

bothPrimary :: DrawingContext -> DrawingContext
bothPrimary = (\s a -> s { secondary_colour = a }) <*> primary_colour

bothSecondary :: DrawingContext -> DrawingContext
bothSecondary = (\s a -> s { primary_colour = a }) <*> secondary_colour



primaryColour :: RGBi -> DrawingContext -> DrawingContext
primaryColour rgb = \s -> s { primary_colour = rgb } 


secondaryColour :: RGBi -> DrawingContext -> DrawingContext
secondaryColour rgb = \s -> s { secondary_colour = rgb } 

