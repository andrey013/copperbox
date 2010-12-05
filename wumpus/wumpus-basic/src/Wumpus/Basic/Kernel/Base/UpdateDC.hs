{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.UpdateDC
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Customize drawing attributes
--
-- \*\* WARNING \*\* - this module needs systematic naming 
-- schemes both for update functions (primaryColour, ...) and 
-- for synthesized selectors (e.g. lowerxHeight). The current 
-- names will change.
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.UpdateDC
  ( 

  -- * Modifiers 
  -- ** Line widths
    lineWidth
  , thick
  , ultrathick
  , thin

  -- ** Line caps
  , capButt
  , capRound
  , capSquare

  -- ** Line joins
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

  -- * Font properties
  , fontAttr
  , fontSize
  , fontFace

  -- * Font / mark drawing size
  , doublesize
  , halfsize

  -- * Colour
  , swapColours
  , bothStrokeColour
  , bothFillColour
  , strokeColour
  , fillColour 


  
  ) where


import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative


--------------------------------------------------------------------------------

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

lineWidth       :: Double -> DrawingContextF
lineWidth d      = updateStrokeProps (\s -> s { line_width = d })

-- | Set the line width to a /thick/.
--
-- Note this context update is /oblivious/ - operationally the 
-- line width is set to exactly @2.0@.
--
thick               :: DrawingContextF
thick               = lineWidth thick_line

ultrathick          :: DrawingContextF
ultrathick          = lineWidth ultra_thick_line

thin                :: DrawingContextF
thin                = lineWidth thin_line


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

fontAttr            :: FontFace -> Int -> DrawingContextF
fontAttr ff sz      = (\s -> s { font_props = FontAttr sz ff })

fontFace            :: FontFace -> DrawingContextF
fontFace ff         = updateFontProps (\(FontAttr sz _) -> FontAttr sz ff)

fontSize            :: Int -> DrawingContextF
fontSize sz         = updateFontProps (\(FontAttr _ ff) -> FontAttr sz ff)


--------------------------------------------------------------------------------

-- | Set the font size to double the current size, note the font
-- size also controls the size of dots, arrowsheads etc.
-- 
doublesize          :: DrawingContextF
doublesize          = (\s sz -> fontSize (sz*2) s) 
                        <*> (font_size . font_props)


-- | Set the font size to half the current size, note the font
-- size also controls the size of dots, arrowsheads etc.
-- 
-- As fontsize is an integer this is not exact - half size of
-- 15pt type is 7pt.
-- 
halfsize            :: DrawingContextF
halfsize            = (\s sz -> fontSize (sz `div` 2) s) 
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



