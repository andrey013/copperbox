{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.DrawingContext
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
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
  , metricsContext

  , default_drawing_context

  -- * Modifiers 
  -- ** Line widths
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
  , fontsize
  , fontface

  -- * Font / mark drawing size
  , doublesize
  , halfsize

  -- * Colour
  , swapColours
  , bothStrokeColour
  , bothFillColour
  , strokeColour
  , fillColour 

  -- * Glyph metrics
  , withFontMetrics

  
  ) where


import Wumpus.Basic.Graphic.GlyphMetrics

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.StandardEncoding

import Control.Applicative
import qualified Data.Map as Map
import Data.Maybe


data DrawingContext = DrawingContext
      { glyph_tables          :: BaseGlyphMetrics
      , fallback_metrics      :: GlyphMetrics      
      , stroke_props          :: StrokeAttr
      , font_props            :: FontAttr
      , stroke_colour         :: RGBi      -- also text colour...
      , fill_colour           :: RGBi      
      , line_spacing_factor   :: Double
      }

-- TODO - hand craft a Show instance 

type DrawingContextF = DrawingContext -> DrawingContext


standardContext :: FontSize -> DrawingContext
standardContext sz = 
    DrawingContext { glyph_tables         = Map.empty
                   , fallback_metrics     = monospace_metrics
                   , stroke_props         = default_stroke_attr
                   , font_props           = FontAttr sz wumpus_courier
                   , stroke_colour        = wumpus_black
                   , fill_colour          = wumpus_light_gray
                   , line_spacing_factor  = 1.2  
                   }


metricsContext :: FontSize -> BaseGlyphMetrics -> DrawingContext
metricsContext sz bgm = 
    DrawingContext { glyph_tables         = bgm
                   , fallback_metrics     = monospace_metrics
                   , stroke_props         = default_stroke_attr
                   , font_props           = FontAttr sz wumpus_courier
                   , stroke_colour        = wumpus_black
                   , fill_colour          = wumpus_light_gray
                   , line_spacing_factor  = 1.2  
                   }




wumpus_black            :: RGBi
wumpus_black            = RGBi 0 0 0 

wumpus_light_gray       :: RGBi
wumpus_light_gray       = RGBi 200 200 200


-- | Courier
-- 
wumpus_courier :: FontFace
wumpus_courier = FontFace "Courier" "Courier New" SVG_REGULAR standard_encoding



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





--------------------------------------------------------------------------------
-- Glyph metrics

-- These are directly on the DrawingContext /for efficiency/.



withFontMetrics :: (GlyphMetrics -> PtSize -> u) -> DrawingContext -> u
withFontMetrics fn ctx@(DrawingContext { font_props = font_stats }) = 
      fn metric_set point_sz
  where 
    ps_name     = ps_font_name $ font_face font_stats
    point_sz    = fromIntegral $ font_size font_stats 
    metric_set  = fromMaybe (fallback_metrics ctx) $ 
                    Map.lookup ps_name (glyph_tables ctx) 


{-
maxGlyphHeight :: FromPtSize u => DrawingContext -> u
maxGlyphHeight = withFontMetrics (\rec sz -> get_max_height rec sz)

avLookupTable :: FromPtSize u => DrawingContext -> (Int -> Vec2 u)
avLookupTable = withFontMetrics (\rec sz -> get_av_lookup rec sz)
-}
