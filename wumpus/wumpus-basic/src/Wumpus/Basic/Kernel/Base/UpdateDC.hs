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
-- DrawingContext modifiers in this module. This is because the 
-- modifiers defined here are expected to be used mostly as static 
-- \"properties\" resembling constants in drawings.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.UpdateDC
  ( 

  -- * Modifiers       
    round_corner_factor
  , snap_grid_factors

  -- ** Line widths
  , set_line_width

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
  , set_dash_pattern

  , solid_line
  , dotted_line
  , packed_dotted
  , loose_dotted
  
  , dashed_line
  , packed_dashed
  , loose_dashed
  
  -- * Font properties
  , font_attr
  , set_font
  , set_font_size


  -- * Font / mark drawing size
  , scale_point_size

  , double_point_size
  , half_point_size

  , text_margin
  , text_margin_none
  , text_margin_tight
  , text_margin_default
  , text_margin_loose


  -- * Colour
  , stroke_colour
  , fill_colour 
  , text_colour
  , single_colour

  , swap_colours
  , fill_use_stroke_colour
  , stroke_use_fill_colour

  -- * Connector Props
  , source_sep
  , dest_sep
  , uniform_conn_sep
  , conn_arc_angle
  , source_arm_len
  , dest_arm_len
  , uniform_arm_len
  , conn_loop_size

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.CtxUnits
import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative


--------------------------------------------------------------------------------
-- helpers 

updateStrokeProps :: (StrokeAttr -> StrokeAttr) -> DrawingContextF
updateStrokeProps fn = 
    (\s i -> s { dc_stroke_props = fn i }) <*> dc_stroke_props


withFontSize :: (FontSize -> DrawingContextF) -> DrawingContextF
withFontSize fn = (\s i -> fn i s) <*> dc_font_size

--------------------------------------------------------------------------------

round_corner_factor   :: Double -> DrawingContextF
round_corner_factor d = \s -> s { dc_round_corner_factor = d }



-- | 'snap_grid_factors' : @ x_unit * y_unit -> DrawingContextF @
-- 
-- Set the @snap grid factors@ - a snap grid is an alternative 
-- coordinate space, it can be convenient for drawing 
-- \"box and arrow\" diagrams.
--
snap_grid_factors   :: Double -> Double -> DrawingContextF
snap_grid_factors xu yu = \s -> s { dc_snap_grid_factors = (xu, yu) }


--------------------------------------------------------------------------------
-- line widths


-- | set_line_width : @ width_in_points -> DrawingContextF @
--
-- Set the line_width to the supplied point size.
--
-- Initially the line width is 1.0.
--
-- /Constant/ variations of the function maybe be more 
-- convenient:
--
-- > line_default, line_thin, line_thick, line_ultra_thick
--
set_line_width      :: Double -> DrawingContextF
set_line_width d    = updateStrokeProps (\s -> s { line_width = d })


-- | Set the line_width to @default@ - 1.0.
--
line_default        :: DrawingContextF
line_default        = set_line_width 1.0


-- | Set the line_width to @thin@ - 0.5.
--
line_thin           :: DrawingContextF
line_thin           = set_line_width 0.5


-- | Set the line_width to @thick@ - 2.0.
--
line_thick          :: DrawingContextF
line_thick          = set_line_width 2.0

-- | Set the line_width to @ultra_thick@ - 4.0.
--
line_ultra_thick    :: DrawingContextF
line_ultra_thick    = set_line_width 4.0


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
set_dash_pattern    :: DashPattern -> DrawingContextF
set_dash_pattern d  = updateStrokeProps (\s -> s { dash_pattern = d })        


-- | Set the dash_pattern to @solid@ - i.e. no dash pattern.
--
-- This is the default.
--
solid_line          :: DrawingContextF 
solid_line          = set_dash_pattern Solid

-- | Set the dash pattern to draw a dotted line.
-- 
-- A dot is actually a square - side length is equal to the line 
-- width.
-- 
-- The spacing between dots is 2 times the dot width.
--
dotted_line         :: DrawingContextF 
dotted_line         = set_dash_pattern $ Dash 0 [(1,2)]

-- | Set the dash pattern to draw a tightly packed dotted line.
-- 
-- A dot is actually a square - side length is equal to the line 
-- width.
-- 
-- The spacing between dots is equal to the dot width.
--
packed_dotted       :: DrawingContextF 
packed_dotted       = set_dash_pattern $ Dash 0 [(1,1)]


-- | Set the dash pattern to draw a loosely dotted line.
-- 
-- A dot is actually a square - side length is equal to the line 
-- width.
-- 
-- The spacing between dots is 4 times the dot width.
--
loose_dotted        :: DrawingContextF 
loose_dotted        = set_dash_pattern $ Dash 0 [(1,4)]



-- | Set the dash pattern to draw a dashed line.
-- 
-- The dash length is 3 times the line width, the spacing is 2
-- times the line width.
--
dashed_line        :: DrawingContextF
dashed_line        = set_dash_pattern $ Dash 0 [(3,2)]


-- | Set the dash pattern to draw a tightly packed, dashed line.
-- 
-- The dash length is 3 times the line width, the spacing is 
-- equal to the line width.
--
packed_dashed      :: DrawingContextF
packed_dashed      = set_dash_pattern $ Dash 0 [(3,1)]


-- | Set the dash pattern to draw a loosely dashed line.
-- 
-- The dash length is 3 times the line width, the spacing is 4
-- times the line width.
--
loose_dashed      :: DrawingContextF
loose_dashed      = set_dash_pattern $ Dash 0 [(3,4)]



--------------------------------------------------------------------------------

-- | Set the font attributes, point size and font face.
--
font_attr               :: FontFace -> Int -> DrawingContextF
font_attr ff sz         = \s -> s { dc_font_size = sz, dc_font_face = ff }

-- | Set the font face.
--
set_font                :: FontFace -> DrawingContextF
set_font ff             = \s -> s { dc_font_face = ff }


-- | Set the point size.
--
-- This controls the drawing size of both 
-- text labels and marks (e.g. dots and arrowheads).
--
set_font_size           :: Int -> DrawingContextF
set_font_size sz        = \s -> s { dc_font_size = sz }


-- | Scale the current point size by the supplied ratio.
-- 
-- Note - as fonts can only be drawn at integral sizes this 
-- operation is not exact - for instance scaling 15pt by (1%2) 
-- results in 7pt.
-- 
scale_point_size    :: Double -> DrawingContextF
scale_point_size a  = (\s sz -> set_font_size (floor $ a * fromIntegral sz) s) 
                         <*> dc_font_size

-- | Set the point size (font and mark size) to double the current 
-- size.
--
double_point_size   :: DrawingContextF
double_point_size   = scale_point_size 2 


-- | Set the point size to half the current size, note the point
-- size also controls the size of dots, arrowsheads etc.
-- 
-- Note - as fonts can only be drawn at integral sizes this 
-- operation is not exact - half size of 15pt type is 7pt.
-- 
half_point_size     :: DrawingContextF
half_point_size     = scale_point_size 0.5


-- | 'text_margin' : @ x_sep * y_sep -> DrawingContextF @
--
-- NOTE - ideally this would use Em or En rather thn Double...
--
text_margin   :: Em -> Em -> DrawingContextF
text_margin xsep ysep = \s -> 
    s { dc_text_margin = TextMargin xsep ysep }


-- | Set the text margin to (0,0).
-- 
-- This produces a tight box around the text vertically measured 
-- to the cap-height and descender. Therefore some characters may 
-- extend outside the margin (e.g. accented capitals like 
-- A-grave).
--
text_margin_none    :: DrawingContextF
text_margin_none    = text_margin 0 0 



-- | Set the text margin to (0.25 em, 0.25 em).
-- 
text_margin_tight       :: DrawingContextF
text_margin_tight       = text_margin 0.25 0.25

-- | Set the text margin to (0.5 em, 0.5 em).
-- 
text_margin_default :: DrawingContextF
text_margin_default = text_margin 0.5 0.5

-- | Set the text margin to (1.0 em, 1.0 em).
-- 
text_margin_loose   :: DrawingContextF
text_margin_loose = text_margin 1.0 1.0


--------------------------------------------------------------------------------

-- | Set the stroke colour.
--
stroke_colour :: RGBi -> DrawingContextF
stroke_colour rgb = \s -> s { dc_stroke_colour = rgb } 


-- | Set the fill colour.
--
fill_colour :: RGBi -> DrawingContextF
fill_colour rgb = \s -> s { dc_fill_colour = rgb } 


-- | Set the text colour.
--
text_colour          :: RGBi -> DrawingContextF
text_colour rgb      = (\s -> s { dc_text_colour = rgb})



-- | Set the stroke, fill and text colours to a single colour.
--
single_colour :: RGBi -> DrawingContextF
single_colour rgb = stroke_colour rgb . fill_colour rgb . text_colour rgb



-- | Swap the stroke colour and fill colours.
--
swap_colours :: DrawingContextF
swap_colours = 
    (\s a b -> s { dc_stroke_colour = b, dc_fill_colour = a })
        <*> dc_stroke_colour <*> dc_fill_colour


-- | Set the fill colour to use the current stroke colour.
--
fill_use_stroke_colour :: DrawingContextF
fill_use_stroke_colour = 
    (\s a -> s { dc_fill_colour = a }) <*> dc_stroke_colour


-- | Set the stroke colour to use the current fill colour.
--
stroke_use_fill_colour :: DrawingContextF
stroke_use_fill_colour = 
    (\s a -> s { dc_stroke_colour = a }) <*> dc_fill_colour


--------------------------------------------------------------------------------
-- Connector props

-- helper 
connectorUpd :: (ConnectorProps -> ConnectorProps) -> DrawingContextF
connectorUpd f = 
   (\s a -> s { dc_connector_props = f a }) <*> dc_connector_props

-- | Normalize to zero if negative.
--
normZero :: (Num u, Ord u) => u -> u
normZero u = if u < 0 then 0 else u

-- | Set the connector source separator.
--
-- The separator is used as a projection along the line formed 
-- between connector points to add spacing if required.
-- 
-- The default value is 0. Negative values are not allowed, they
-- are normalized to 0.
--
source_sep :: (Ord u, InterpretUnit u) => u -> DrawingContextF
source_sep u = withFontSize $ \sz -> 
    connectorUpd (\s -> s { dc_conn_src_arm = uconvertScalar sz $ normZero u })


-- | Set the connector destination separator.
--
-- The separator is used as a projection along the line formed 
-- between connector points to add spacing if required.
-- 
-- The default value is 0. Negative values are not allowed, they
-- are normalized to 0.
--
dest_sep :: (Ord u, InterpretUnit u) => u -> DrawingContextF
dest_sep u = withFontSize $ \sz -> 
    connectorUpd (\s -> s { dc_conn_dst_arm = uconvertScalar sz $ normZero u })



-- | Set the connector source and destination separators to the 
-- same length.
--
uniform_conn_sep :: (Ord u, InterpretUnit u) => u -> DrawingContextF
uniform_conn_sep u = withFontSize $ \sz -> 
    connectorUpd (\s -> s { dc_conn_src_arm = uconvertScalar sz $ normZero u
                          , dc_conn_dst_arm = uconvertScalar sz $ normZero u })


-- | Set the connector arc angle.
--
conn_arc_angle :: Radian -> DrawingContextF
conn_arc_angle ang = 
    connectorUpd (\s -> s { dc_conn_arc_ang = ang })


-- | Set the connector source arm length.
--
source_arm_len :: InterpretUnit u => u -> DrawingContextF
source_arm_len u = withFontSize $ \sz -> 
    connectorUpd (\s -> s { dc_conn_src_arm = uconvertScalar sz u })
                       



-- | Set the connector destination arm length.
--
dest_arm_len :: InterpretUnit u => u -> DrawingContextF
dest_arm_len u = withFontSize $ \sz -> 
    connectorUpd (\s -> s { dc_conn_dst_arm = uconvertScalar sz u })




-- | Set the connector source and destination arms to the same 
-- length.
--
uniform_arm_len :: InterpretUnit u => u -> DrawingContextF
uniform_arm_len u = withFontSize $ \sz -> 
    connectorUpd (\s -> s { dc_conn_src_arm = uconvertScalar sz u
                          , dc_conn_dst_arm = uconvertScalar sz u })


-- | Set the connector loop size.
--
conn_loop_size :: InterpretUnit u => u -> DrawingContextF
conn_loop_size u = withFontSize $ \sz -> 
    connectorUpd (\s -> s { dc_conn_loop_size = uconvertScalar sz u })
