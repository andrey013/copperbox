{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.UpdateDC
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Customize drawing attributes. The functions here are 
-- @DrawingContext@ modifiers to be run within a the scope of a 
-- @localize@ block (cf. @local@ of the Reader monad).
--
-- By convention, underscore-separated names are used for 
-- DrawingContext modifiers that take no extra arguments.  
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
    addFontTables

      
  , roundCornerFactor
  , textMargin
  , snapGrid

  -- ** Line widths
  , lineWidth
  , line_default
  , line_thin
  , line_thick
  , line_ultra_thick

  -- ** Line caps
  , cap_default
  , cap_butt
  , cap_round
  , cap_square

  -- ** Line joins
  , join_default
  , join_miter
  , join_round
  , join_bevel

  -- ** Dash Pattern
  , dashPattern
  , solid_line

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
  , scalesize
  , doublesize
  , halfsize

  -- * Colour
  , swap_colours
  , stroke_colour
  , fill_colour 
  , text_colour


  
  ) where


import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.FontMetrics
import Wumpus.Basic.Kernel.Base.Units

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

import Data.Monoid
import Data.Ratio

--------------------------------------------------------------------------------



-- | 'addFontTables' : @ font_load_result -> DrawinContextUpdate @
--
-- Add the font metrics from the FontLoadResult, if a font with 
-- the same name alreay exists in the 'DrawingContext' it will be 
-- replaced. Error and warning messages in the @font_load_result@ 
-- will be appended to the 'font_load_log'.
--
addFontTables :: FontLoadResult -> DrawingContextF
addFontTables (FontLoadResult table msgs) = 
    (\s i j -> s { dc_font_metrics_table = i `mappend` table
                 , dc_font_load_log      = j `mappend` msgs }) 
      <*> dc_font_metrics_table <*> dc_font_load_log


--------------------------------------------------------------------------------
-- helpers 

updateStrokeProps :: (StrokeAttr -> StrokeAttr) -> DrawingContextF
updateStrokeProps fn = 
    (\s i -> s { dc_stroke_props = fn i }) <*> dc_stroke_props

updateFontProps :: (FontAttr -> FontAttr) -> DrawingContextF
updateFontProps fn = 
    (\s i -> s { dc_font_props = fn i }) <*> dc_font_props


--------------------------------------------------------------------------------

roundCornerFactor   :: ToPtSize u => u -> DrawingContextF
roundCornerFactor d = \s -> s { dc_round_corner_factor = toPtSize d }

-- | 'textMargin' : @ x_sep * y_sep -> DrawingContextF @
--
textMargin   :: ToPtSize u => u -> u -> DrawingContextF
textMargin xsep ysep = \s -> 
    s { dc_text_margin = TextMargin (toPtSize xsep) (toPtSize ysep) }


-- | 'snapGrid' : @ x_unit * y_unit -> DrawingContextF @
-- 
-- Modify the 'snap_grid_factors'.
--
snapGrid   :: ToPtSize u => u -> u -> DrawingContextF
snapGrid xu yu = \s -> 
    s { dc_snap_grid_factors = (toPtSize xu, toPtSize yu) }


--------------------------------------------------------------------------------
-- line widths

-- Note - some care might be needed if we ever define other unit 
-- types...

-- | Set the line_width.
--
-- Initially the line width is 1.0.
--
lineWidth       :: Pt -> DrawingContextF
lineWidth d      = updateStrokeProps (\s -> s { line_width = d })

-- | Set the line_width to @default@ - 1.0.
--
line_default        :: DrawingContextF
line_default        = lineWidth 1.0


-- | Set the line_width to @thin@ - 0.5.
--
line_thin           :: DrawingContextF
line_thin           = lineWidth 0.5


-- | Set the line_width to @thick@ - 2.0.
--
line_thick          :: DrawingContextF
line_thick          = lineWidth 2.0

-- | Set the line_width to @ultra_thick@ - 4.0.
--
line_ultra_thick    :: DrawingContextF
line_ultra_thick    = lineWidth 4.0


--
-- All options share the prefix so the enumeration is obvious...
--


--------------------------------------------------------------------------------
-- Line props

setLineCap          :: LineCap -> DrawingContextF
setLineCap d        = updateStrokeProps (\s -> s { line_cap = d })

setLineJoin         :: LineJoin -> DrawingContextF
setLineJoin d       = updateStrokeProps (\s -> s { line_join = d })


-- | Set the line_cap to the default which is @butt@.
--
-- This is a synonym for 'cap_butt'.
--
cap_default         :: DrawingContextF
cap_default         = cap_butt

-- | Set the line_cap to @butt@.
--
-- Butt squares of the stroke at the end point.
--
-- This is the default.
--
cap_butt            :: DrawingContextF
cap_butt            = setLineCap CapButt

-- | Set the line_cap to @round@.
--
-- This rounds the end of the stroke and the visually the 
-- rounding slightly extends the length of the line.
--
cap_round           :: DrawingContextF
cap_round           = setLineCap CapRound


-- | Set the line_cap to @square@.
--
-- This squares off the end of the stroke, visually extending 
-- the stroke by half the line width.
--
cap_square          :: DrawingContextF
cap_square          = setLineCap CapSquare



-- | Set the line_join to the default which is @miter@.
--
-- This is a synonym for 'join_miter'.
--
join_default        :: DrawingContextF
join_default        = join_miter


-- | Set the line_join to @miter@.
--
-- This extends the joining line segments to form a sharp miter.
--
-- This is the default.
--
join_miter          :: DrawingContextF
join_miter          = setLineJoin JoinMiter


-- | Set the line_join to @round@.
--
-- This rounds off the corner of the joined line segments.
--
join_round          :: DrawingContextF
join_round          = setLineJoin JoinRound


-- | Set the line_join to @round@.
--
-- This bevels off the corner of the joined line segments with a 
-- notch.
--
join_bevel          :: DrawingContextF
join_bevel          = setLineJoin JoinBevel


--------------------------------------------------------------------------------

 
-- | Set the dash pattern.
--
-- Initially the dash pattern is 'Solid'.
--
dashPattern         :: DashPattern -> DrawingContextF
dashPattern d       = updateStrokeProps (\s -> s { dash_pattern = d })        


-- | Set the dash_pattern to @solid@ - i.e. no dash pattern.
--
-- This is the default.
--
solid_line          :: DrawingContextF 
solid_line          = dashPattern Solid

-- Note - these are pendening revision...

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
fontAttr ff sz      = (\s -> s { dc_font_props = FontAttr sz ff })

fontFace            :: FontFace -> DrawingContextF
fontFace ff         = updateFontProps (\(FontAttr sz _) -> FontAttr sz ff)

fontSize            :: Int -> DrawingContextF
fontSize sz         = updateFontProps (\(FontAttr _ ff) -> FontAttr sz ff)



--------------------------------------------------------------------------------

scalesize           :: Ratio Int -> DrawingContextF
scalesize r         = let (n,d) = (numerator r, denominator r)
                      in (\s sz -> fontSize (n * sz `div` d) s) 
                           <*> (font_size . dc_font_props)

-- | Set the font size to double the current size, note the font
-- size also controls the size of dots, arrowsheads etc.
-- 
doublesize          :: DrawingContextF
doublesize          = scalesize 2 


-- | Set the font size to half the current size, note the font
-- size also controls the size of dots, arrowsheads etc.
-- 
-- As fontsize is an integer this is not exact - half size of
-- 15pt type is 7pt.
-- 
halfsize            :: DrawingContextF
halfsize            = scalesize (1%2)


-- Something like...
--
-- > mark_half_size
-- > mark_double_size


--------------------------------------------------------------------------------

swap_colours :: DrawingContextF
swap_colours = 
    (\s a b -> s { dc_stroke_colour = b, dc_fill_colour = a })
        <*> dc_stroke_colour <*> dc_fill_colour

{-
bothStrokeColour :: DrawingContextF
bothStrokeColour = (\s a -> s { dc_fill_colour = a }) <*> dc_stroke_colour

bothFillColour :: DrawingContextF
bothFillColour = (\s a -> s { dc_stroke_colour = a }) <*> dc_fill_colour
-}


stroke_colour :: RGBi -> DrawingContextF
stroke_colour rgb = \s -> s { dc_stroke_colour = rgb } 


fill_colour :: RGBi -> DrawingContextF
fill_colour rgb = \s -> s { dc_fill_colour = rgb } 



text_colour          :: RGBi -> DrawingContextF
text_colour rgb      = (\s -> s { dc_text_colour = rgb})


