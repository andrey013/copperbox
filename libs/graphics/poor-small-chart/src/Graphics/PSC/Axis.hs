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
  , AxisLabelDrawF

  , xAxisText 
  , yAxisText
  , drawAxes

  -- * Grids
  , GridConfig(..)
  , simpleGridLine

  , drawGrid


  -- * Border
  , BorderF
  , plainBorder

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
--

type AxisLabelDrawF u = u -> DPoint2 -> Graphic

data AxisLabelAlg u = AxisLabelAlg
      { start_value     :: u
      , step_fun        :: u -> u
      }




xAxisText :: (DRGB,FontAttr) -> Double -> (u -> String) -> AxisLabelDrawF u
xAxisText font_props gap textF = \u north_pt -> 
    wrapH $ textlabelN font_props (textF u) (north_pt .-^ vvec gap)


yAxisText :: (DRGB,FontAttr) -> Double -> (v -> String) -> AxisLabelDrawF v
yAxisText font_props gap textF = \v east_pt -> 
    wrapH $ textlabelE font_props (textF v) (east_pt .-^ hvec gap)


drawAxes :: (u -> Double, v -> Double) 
         -> AxisLabelConfig u v
         -> DrawingRectangle
         -> Graphic
drawAxes (fX,fY) (AxisLabelConfig {x_axis_cfg, y_axis_cfg}) rect = hf . vf
  where
    hf = maybe id (\z -> horizontalLabels fX z rect) x_axis_cfg
    vf = maybe id (\z -> verticalLabels   fY z rect) y_axis_cfg
    

horizontalLabels :: (u -> Double) 
                 -> (AxisLabelAlg u, AxisLabelDrawF u)
                 -> DrawingRectangle 
                 -> Graphic
horizontalLabels fX (axis_alg,buildF) draw_rect = 
    horizontalPoints buildF 0 fX axis_alg draw_rect


verticalLabels :: (v -> Double) 
               -> (AxisLabelAlg v, AxisLabelDrawF v)
               -> DrawingRectangle 
               -> Graphic
verticalLabels fY (axis_alg,buildF) draw_rect = 
    verticalPoints buildF 0 fY axis_alg draw_rect


-- How about a variant of textlabel with position as an arg?
-- (bl, center, east...)

--------------------------------------------------------------------------------
-- Grids


type GridLineDrawF = DPoint2 -> DPoint2 -> Graphic


simpleGridLine :: Stroke t => t -> GridLineDrawF
simpleGridLine t = \p0 p1 -> wrapH $ ostroke t $ path p0 [ lineTo p1 ]

data GridConfig u v = GridConfig
      { grid_line_draw  :: GridLineDrawF
      , grid_x_axis     :: Maybe (AxisLabelAlg u)
      , grid_y_axis     :: Maybe (AxisLabelAlg v)
      } 



drawGrid :: (u -> Double, v -> Double) 
         -> GridConfig u v
         -> DrawingRectangle
         -> Graphic
drawGrid (fX,fY) (GridConfig {grid_line_draw, grid_x_axis, grid_y_axis}) rect =
    vf . hf
  where
    hf = maybe id (\alg -> horizontalLines fY grid_line_draw alg rect) grid_y_axis
    vf = maybe id (\alg -> verticalLines   fX grid_line_draw alg rect) grid_x_axis


verticalLines :: (u -> Double)
              -> GridLineDrawF
              -> AxisLabelAlg u
              -> DrawingRectangle
              -> Graphic
verticalLines fX drawF axis_alg draw_rect = 
    horizontalPoints buildF 0 fX axis_alg draw_rect
  where
    buildF _ pt  = drawF pt (pt .+^ upvec)
    upvec        = vvec $ rect_height draw_rect


horizontalLines :: (v -> Double) 
                -> GridLineDrawF
                -> AxisLabelAlg v
                -> DrawingRectangle
                -> Graphic
horizontalLines fY drawF axis_alg draw_rect = 
    verticalPoints buildF 0 fY axis_alg draw_rect
  where
    buildF _ pt = drawF pt (pt .+^ rightvec)
    rightvec    = hvec $ rect_width draw_rect



--------------------------------------------------------------------------------
-- Enumerate x-y values, generate points

horizontalPoints :: (u -> DPoint2 -> Graphic) 
                 -> Double 
                 -> (u -> Double) 
                 -> AxisLabelAlg u 
                 -> DrawingRectangle 
                 -> Graphic
horizontalPoints buildF y0 fX axis_alg rect = 
    veloH (\(xu,x) -> buildF xu (P2 x y0)) $ xvalues fX axis_alg rect


verticalPoints :: (v -> DPoint2 -> Graphic) 
               -> Double 
               -> (v -> Double) 
               -> AxisLabelAlg v
               -> DrawingRectangle 
               -> Graphic 
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


--------------------------------------------------------------------------------

type BorderF = DPoint2 -> DPoint2 -> Graphic


plainBorder :: DRGB -> Double -> BorderF
plainBorder rgb lw = \bl@(P2 x0 y0) (P2 x1 y1)  -> wrapH $ 
    cstroke (rgb, LineWidth lw) $ vertexPath $ rectPoints (x1-x0) (y1-y0) bl