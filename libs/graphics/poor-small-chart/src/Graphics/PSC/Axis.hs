{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Axis
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC - NamedFieldPuns
--
-- Axes / grids
--
--------------------------------------------------------------------------------

module Graphics.PSC.Axis
  ( 
  -- * Axes
    AxisLabelConfig(..)
  , AxisLabelAlg(..)
  , drawAxes

  -- * Grids
  , GridConfig(..)
  , drawGrid

  ) where

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils
import Graphics.PSC.Utils

import Wumpus.Core                      -- package: wumpus-core

-- import Data.AffineSpace                 -- package: vector-space 



data AxisLabelConfig u v = AxisLabelConfig
      { axis_label_cfg  :: LabelConfig
      , x_axis_cfg      :: Maybe (AxisLabelAlg u, u -> String)
      , y_axis_cfg      :: Maybe (AxisLabelAlg v, v -> String)
      } 

-- How you draw axis labels is quite "shrewd" - i.e 
-- ticks / labels or both...
-- It\'s probably better to make the construct a function
-- from Point -> Drawing than try to stor its components.
--


data AxisLabelAlg u = AxisLabelAlg
      { start_value     :: u
      , step_fun        :: u -> u
      }




-- NOTE - need a bit more sophistication to offest labels...

drawAxes :: (u -> Double, v -> Double) 
         -> AxisLabelConfig u v
         -> DrawingRectangle
         -> [DPrimitive]
drawAxes (fX,fY) axis_cfg@(AxisLabelConfig {x_axis_cfg, y_axis_cfg}) rect =
    toListH $ hf $ vf emptyH
  where
    hf = maybeHf (\z -> horizontalLabels fX font_attr z rect) x_axis_cfg
    vf = maybeHf (\z -> verticalLabels   fY font_attr z rect) y_axis_cfg
    
    font_attr = fontProps axis_cfg

fontProps :: AxisLabelConfig u v -> (DRGB,FontAttr)
fontProps (AxisLabelConfig {axis_label_cfg}) = 
    (label_text_colour axis_label_cfg, label_font axis_label_cfg)

    

horizontalLabels :: (u -> Double) 
                 -> (DRGB,FontAttr) 
                 -> (AxisLabelAlg u, u -> String)
                 -> DrawingRectangle 
                 -> [DPrimitive]
horizontalLabels fX font_props (axis_alg,textF) draw_rect = 
    zipWith tf points strs_inf
  where
    points      = horizontalPoints 0 fX axis_alg draw_rect
    strs_inf    = map textF $ infValues axis_alg         
    tf pt lbl   = textlabelN font_props lbl pt


verticalLabels :: (v -> Double) 
               -> (DRGB,FontAttr) 
               -> (AxisLabelAlg v, v -> String)
               -> DrawingRectangle 
               -> [DPrimitive]
verticalLabels fY font_props (axis_alg,textF) draw_rect = 
    zipWith tf points strs_inf
  where
    points      = verticalPoints 0 fY axis_alg draw_rect
    strs_inf    = map textF $ infValues axis_alg         
    tf pt lbl   = textlabelE font_props lbl pt

-- How about a variant of textlabel with position as an arg?
-- (bl, center, east...)

--------------------------------------------------------------------------------
-- Grids


data GridConfig u v = GridConfig
      { grid_line       :: LineConfig
      , grid_x_axis     :: Maybe (AxisLabelAlg u)
      , grid_y_axis     :: Maybe (AxisLabelAlg v)
      } 



drawGrid :: (u -> Double, v -> Double) 
         -> GridConfig u v
         -> DrawingRectangle
         -> [DPrimitive]
drawGrid (fX,fY) (GridConfig {grid_line, grid_x_axis, grid_y_axis}) rect =
    toListH $ vf $ hf emptyH
  where
    hf = maybeHf (\alg -> horizontalLines fY grid_line alg rect) grid_y_axis
    vf = maybeHf (\alg -> verticalLines   fX grid_line alg rect) grid_x_axis


maybeHf :: (a -> [b]) -> Maybe a -> (H b -> H b)
maybeHf _ Nothing  = id
maybeHf f (Just a) = appendH (fromListH $ f a) 



verticalLines :: (u -> Double)
              -> LineConfig
              -> AxisLabelAlg u
              -> DrawingRectangle
              -> [DPrimitive]
verticalLines fX line_cfg axis_alg draw_rect = 
    map (\pt -> sf $ straightLine pt upvec) points
  where
    points  = horizontalPoints 0 fX axis_alg draw_rect
    upvec   = vvec $ rect_height draw_rect
    sf      = ostroke (makeStrokeProps line_cfg)


horizontalLines :: (v -> Double) 
                -> LineConfig
                -> AxisLabelAlg v
                -> DrawingRectangle
                -> [DPrimitive]
horizontalLines fY line_cfg axis_alg draw_rect = 
    map (\pt -> sf $ straightLine pt rightvec) points
  where
    points      = verticalPoints 0 fY axis_alg draw_rect
    rightvec    = hvec $ rect_width draw_rect
    sf          = ostroke (makeStrokeProps line_cfg)



--------------------------------------------------------------------------------
-- Enumerate x-y values, generate points

horizontalPoints :: Double 
                 -> (u -> Double) 
                 -> AxisLabelAlg u 
                 -> DrawingRectangle 
                 -> [DPoint2]
horizontalPoints y0 fX axis_alg rect = 
    map (\x -> P2 x y0) $ xvalues fX axis_alg rect


verticalPoints :: Double 
               -> (v -> Double) 
               -> AxisLabelAlg v
               -> DrawingRectangle 
               -> [DPoint2]
verticalPoints x0 fY axis_alg rect = 
    map (\y -> P2 x0 y) $ yvalues fY axis_alg rect


xvalues :: (u -> Double) ->  AxisLabelAlg u -> DrawingRectangle -> [Double]
xvalues fX alg draw_rect = takeWhile cmp $ map fX $ infValues alg
  where
    cmp x = x `leqEps` rect_width draw_rect 

yvalues :: (v -> Double) ->  AxisLabelAlg v -> DrawingRectangle -> [Double]
yvalues fY alg draw_rect = takeWhile cmp $ map fY $ infValues alg
  where
    cmp y = y `leqEps` rect_height draw_rect

infValues :: AxisLabelAlg unit -> [unit]
infValues (AxisLabelAlg {start_value,step_fun}) = iterate step_fun start_value 


leqEps :: Double -> Double -> Bool
leqEps a b | a < b     = True
           | otherwise = let diff = a - b in diff < rect_epsilon 

rect_epsilon :: Double 
rect_epsilon = 0.01