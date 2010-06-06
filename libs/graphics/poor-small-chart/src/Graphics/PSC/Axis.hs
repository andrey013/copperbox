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
  , AxisSteps
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
      { x_axis_cfg      :: (AxisSteps u, AxisLabelDrawF u)
      , y_axis_cfg      :: (AxisSteps v, AxisLabelDrawF v)
      } 

-- How you draw axis labels is quite "shrewd" - i.e 
-- ticks / labels or both, or neither...
--

type AxisLabelDrawF u = u -> DPoint2 -> Graphic

type AxisSteps u = [u]


xAxisText :: (DRGB,FontAttr) -> Double -> (u -> String) -> AxisLabelDrawF u
xAxisText font_props gap textF = \u north_pt -> 
    textlabelN font_props (textF u) (north_pt .-^ vvec gap)


yAxisText :: (DRGB,FontAttr) -> Double -> (v -> String) -> AxisLabelDrawF v
yAxisText font_props gap textF = \v east_pt -> 
    textlabelE font_props (textF v) (east_pt .-^ hvec gap)


drawAxes :: (AxisSteps u, AxisLabelDrawF u)
         -> (AxisSteps v, AxisLabelDrawF v)
         -> (u -> Double, v -> Double) 
         -> DrawingRectangle
         -> Graphic
drawAxes (usteps,udrawF) (vsteps,vdrawF) (fX,fY) rect = hf . vf
  where
    hf = horizontalLabels usteps udrawF fX rect 
    vf = verticalLabels   vsteps vdrawF fY rect
    

horizontalLabels :: AxisSteps u
                 -> AxisLabelDrawF u
                 -> (u -> Double)
                 -> DrawingRectangle 
                 -> Graphic
horizontalLabels steps buildF fX draw_rect = 
    horizontals buildF 0 fX steps draw_rect


verticalLabels :: AxisSteps v
               -> AxisLabelDrawF v
               -> (v -> Double)
               -> DrawingRectangle 
               -> Graphic
verticalLabels steps buildF fY draw_rect = 
    verticals buildF 0 fY steps draw_rect


--------------------------------------------------------------------------------
-- Grids


type StraightLineF = DPoint2 -> DPoint2 -> Graphic


simpleGridLine :: Stroke t => t -> StraightLineF
simpleGridLine t = \p0 p1 -> straightLine t (p1 .-. p0) p0

data GridConfig u v = GridConfig
      { grid_line_draw  :: StraightLineF
      , grid_x_axis     :: Maybe (AxisSteps u)
      , grid_y_axis     :: Maybe (AxisSteps v)
      } 



drawGrid :: (u -> Double, v -> Double) 
         -> GridConfig u v
         -> DrawingRectangle
         -> Graphic
drawGrid (fX,fY) (GridConfig {grid_line_draw, grid_x_axis, grid_y_axis}) rect =
    vf . hf
  where
    hf = maybe id (\steps -> horizontalLines fY grid_line_draw steps rect) grid_y_axis
    vf = maybe id (\steps -> verticalLines   fX grid_line_draw steps rect) grid_x_axis


verticalLines :: (u -> Double)
              -> StraightLineF
              -> AxisSteps u
              -> DrawingRectangle
              -> Graphic
verticalLines fX drawF steps draw_rect = 
    horizontals buildF 0 fX steps draw_rect
  where
    buildF _ pt  = drawF pt (pt .+^ upvec)
    upvec        = vvec $ rect_height draw_rect


horizontalLines :: (v -> Double) 
                -> StraightLineF
                -> AxisSteps v
                -> DrawingRectangle
                -> Graphic
horizontalLines fY drawF steps draw_rect = 
    verticals buildF 0 fY steps draw_rect
  where
    buildF _ pt = drawF pt (pt .+^ rightvec)
    rightvec    = hvec $ rect_width draw_rect



--------------------------------------------------------------------------------
-- Enumerate x-y values...

horizontals :: (u -> DPoint2 -> Graphic) 
            -> Double 
            -> (u -> Double) 
            -> AxisSteps u 
            -> DrawingRectangle 
            -> Graphic
horizontals buildF y0 fX steps rect = 
    veloH (\(xu,x) -> buildF xu (P2 x y0)) $ xvalues fX steps rect


verticals :: (v -> DPoint2 -> Graphic) 
          -> Double 
          -> (v -> Double) 
          -> AxisSteps v
          -> DrawingRectangle 
          -> Graphic 
verticals buildF x0 fY steps rect = 
    veloH (\(yu,y) -> buildF yu (P2 x0 y)) $ yvalues fY steps rect



xvalues :: (u -> Double) ->  AxisSteps u -> DrawingRectangle -> [(u,Double)]
xvalues fX steps draw_rect = takeWhile cmp $ map (\a -> (a,fX a)) steps
  where
    cmp (_,x) = x `leqEps` rect_width draw_rect 

yvalues :: (v -> Double) ->  AxisSteps v -> DrawingRectangle -> [(v,Double)]
yvalues fY steps draw_rect = takeWhile cmp $ map (\a -> (a, fY a)) steps
  where
    cmp (_,y) = y `leqEps` rect_height draw_rect



leqEps :: Double -> Double -> Bool
leqEps a b | a < b     = True
           | otherwise = let diff = a - b in diff < rect_epsilon 

rect_epsilon :: Double 
rect_epsilon = 0.01


--------------------------------------------------------------------------------

type BorderF = DPoint2 -> DPoint2 -> Graphic


plainBorder :: DRGB -> Double -> BorderF
plainBorder rgb lw = \bl@(P2 x0 y0) (P2 x1 y1) -> 
    strokedRectangle (rgb, LineWidth lw) (x1-x0) (y1-y0) bl