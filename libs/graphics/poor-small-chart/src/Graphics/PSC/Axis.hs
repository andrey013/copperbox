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

  , xAxisText 
  , yAxisText
  , drawAxes

  -- * Grids
  , GridConfig(..)
  , drawGrid

  ) where

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils
import Graphics.PSC.Utils

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space 



data AxisLabelConfig u v = AxisLabelConfig
      { x_axis_cfg      :: Maybe (AxisLabelAlg u, AxisLabelDrawF u)
      , y_axis_cfg      :: Maybe (AxisLabelAlg v, AxisLabelDrawF v)
      } 

-- How you draw axis labels is quite "shrewd" - i.e 
-- ticks / labels or both, or neither...
-- It\'s probably better to make the construct a function
-- from Point -> Drawing than try to stor its components.
--

type AxisLabelDrawF u = u -> DPoint2 -> HPrim Double

data AxisLabelAlg u = AxisLabelAlg
      { start_value     :: u
      , step_fun        :: u -> u
      }




-- NOTE - need a bit more sophistication to offest labels...


xAxisText :: (DRGB,FontAttr) -> Double -> (u -> String) -> AxisLabelDrawF u
xAxisText font_props gap textF = \u north_pt -> 
    wrapH $ textlabelN font_props (textF u) (north_pt .-^ vvec gap)


yAxisText :: (DRGB,FontAttr) -> Double -> (v -> String) -> AxisLabelDrawF v
yAxisText font_props gap textF = \v east_pt -> 
    wrapH $ textlabelE font_props (textF v) (east_pt .-^ hvec gap)


drawAxes :: (u -> Double, v -> Double) 
         -> AxisLabelConfig u v
         -> DrawingRectangle
         -> Maybe DPicture
drawAxes (fX,fY) (AxisLabelConfig {x_axis_cfg, y_axis_cfg}) rect =
    drawHPrim $ hf . vf
  where
    hf = maybe id (\z -> horizontalLabels fX z rect) x_axis_cfg
    vf = maybe id (\z -> verticalLabels   fY z rect) y_axis_cfg
    

horizontalLabels :: (u -> Double) 
                 -> (AxisLabelAlg u, AxisLabelDrawF u)
                 -> DrawingRectangle 
                 -> HPrim Double
horizontalLabels fX (axis_alg,buildF) draw_rect = 
    horizontalPoints buildF 0 fX axis_alg draw_rect


verticalLabels :: (v -> Double) 
               -> (AxisLabelAlg v, AxisLabelDrawF v)
               -> DrawingRectangle 
               -> HPrim Double
verticalLabels fY (axis_alg,buildF) draw_rect = 
    verticalPoints buildF 0 fY axis_alg draw_rect


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
         -> Maybe DPicture
drawGrid (fX,fY) (GridConfig {grid_line, grid_x_axis, grid_y_axis}) rect =
    drawHPrim $ vf . hf
  where
    hf = maybe id (\alg -> horizontalLines fY grid_line alg rect) grid_y_axis
    vf = maybe id (\alg -> verticalLines   fX grid_line alg rect) grid_x_axis


verticalLines :: (u -> Double)
              -> LineConfig
              -> AxisLabelAlg u
              -> DrawingRectangle
              -> HPrim Double
verticalLines fX line_cfg axis_alg draw_rect = 
    horizontalPoints buildF 0 fX axis_alg draw_rect
  where
    buildF _ pt  = wrapH $ sf $ straightLine pt upvec
    upvec        = vvec $ rect_height draw_rect
    sf           = ostroke (makeStrokeProps line_cfg)


horizontalLines :: (v -> Double) 
                -> LineConfig
                -> AxisLabelAlg v
                -> DrawingRectangle
                -> HPrim Double
horizontalLines fY line_cfg axis_alg draw_rect = 
    verticalPoints buildF 0 fY axis_alg draw_rect
  where
    buildF _ pt = wrapH $ sf $ straightLine pt rightvec
    rightvec    = hvec $ rect_width draw_rect
    sf          = ostroke (makeStrokeProps line_cfg)



--------------------------------------------------------------------------------
-- Enumerate x-y values, generate points

horizontalPoints :: (u -> DPoint2 -> H a) 
                 -> Double 
                 -> (u -> Double) 
                 -> AxisLabelAlg u 
                 -> DrawingRectangle 
                 -> H a
horizontalPoints buildF y0 fX axis_alg rect = 
    veloH (\(xu,x) -> buildF xu (P2 x y0)) $ xvalues fX axis_alg rect


verticalPoints :: (v -> DPoint2 -> H a) 
               -> Double 
               -> (v -> Double) 
               -> AxisLabelAlg v
               -> DrawingRectangle 
               -> H a 
verticalPoints buildF x0 fY axis_alg rect = 
    veloH (\(yu,y) -> buildF yu (P2 x0 y)) $ yvalues fY axis_alg rect



xvalues :: (u -> Double) ->  AxisLabelAlg u -> DrawingRectangle -> [(u,Double)]
xvalues fX alg draw_rect = takeWhile cmp $ map (\a -> (a,fX a)) $ infValues alg
  where
    cmp (_,x) = x `leqEps` rect_width draw_rect 

yvalues :: (v -> Double) ->  AxisLabelAlg v -> DrawingRectangle -> [(v,Double)]
yvalues fY alg draw_rect = takeWhile cmp $ map (\a -> (a, fY a)) $ infValues alg
  where
    cmp (_,y) = y `leqEps` rect_height draw_rect

infValues :: AxisLabelAlg u -> [u]
infValues (AxisLabelAlg {start_value,step_fun}) = iterate step_fun start_value 


leqEps :: Double -> Double -> Bool
leqEps a b | a < b     = True
           | otherwise = let diff = a - b in diff < rect_epsilon 

rect_epsilon :: Double 
rect_epsilon = 0.01