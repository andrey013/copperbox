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
      { x_axis_cfg      :: (AxisLabelDrawF u, AxisSteps u)
      , y_axis_cfg      :: (AxisLabelDrawF v, AxisSteps v)
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


drawAxes :: (AxisLabelDrawF u, AxisSteps u)
         -> (AxisLabelDrawF v, AxisSteps v)
         -> ScaleCtx u v Graphic
drawAxes (udrawF,usteps) (vdrawF,vsteps) ctx = hf . vf
  where
    hf = horizontalLabels udrawF usteps ctx 
    vf = verticalLabels   vdrawF vsteps ctx
    

horizontalLabels :: AxisLabelDrawF u -> AxisSteps u -> ScaleCtx u v Graphic
horizontalLabels buildF steps = horizontals 0 buildF steps


verticalLabels :: AxisLabelDrawF v -> AxisSteps v -> ScaleCtx u v Graphic
verticalLabels buildF steps = verticals 0 buildF steps 


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



drawGrid :: GridConfig u v -> ScaleCtx u v Graphic
drawGrid (GridConfig {grid_line_draw, grid_x_axis, grid_y_axis}) ctx =
    vf . hf
  where
    hf = maybe id (\steps -> horizontalLines grid_line_draw steps ctx) grid_y_axis
    vf = maybe id (\steps -> verticalLines   grid_line_draw steps ctx) grid_x_axis


verticalLines :: StraightLineF
              -> AxisSteps u
              -> ScaleCtx u v Graphic
verticalLines drawF steps ctx@(rect,_,_) = horizontals 0 buildF steps ctx
  where
    buildF _ pt  = drawF pt (pt .+^ upvec)
    upvec        = vvec $ rect_height rect


horizontalLines :: StraightLineF
                -> AxisSteps v
                -> ScaleCtx u v Graphic
horizontalLines drawF steps ctx@(rect,_,_) = verticals 0 buildF steps ctx
  where
    buildF _ pt = drawF pt (pt .+^ rightvec)
    rightvec    = hvec $ rect_width rect



--------------------------------------------------------------------------------
-- Enumerate x-y values...

horizontals :: Double 
            -> (u -> DPoint2 -> Graphic) 
            -> AxisSteps u 
            -> ScaleCtx u v Graphic
horizontals y0 buildF steps ctx = 
    veloH (\(xu,x) -> buildF xu (P2 x y0)) $ xvalues steps ctx


verticals :: Double 
          -> (v -> DPoint2 -> Graphic) 
          -> AxisSteps v 
          -> ScaleCtx u v Graphic 
verticals x0 buildF steps ctx = 
    veloH (\(yu,y) -> buildF yu (P2 x0 y)) $ yvalues steps ctx



xvalues :: AxisSteps u -> ScaleCtx u v [(u,Double)]
xvalues steps (rect,fX,_) = takeWhile cmp $ map (\a -> (a,fX a)) steps
  where
    cmp (_,x) = x `leqEps` rect_width rect 

yvalues :: AxisSteps v -> ScaleCtx u v [(v,Double)]
yvalues steps (rect,_,fY) = takeWhile cmp $ map (\a -> (a, fY a)) steps
  where
    cmp (_,y) = y `leqEps` rect_height rect



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