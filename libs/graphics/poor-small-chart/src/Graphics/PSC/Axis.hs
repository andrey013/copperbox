{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Axis
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Axes / grids
--
--------------------------------------------------------------------------------

module Graphics.PSC.Axis
  ( 
  -- * Axes
    AxisF
  , AxisSteps
  , AxisLabelDrawF
 
  , xAxisText 
  , yAxisText
  , drawAxes

  -- * Grids
  , GridF 
  , StraightLineF
  , simpleGridLine

  , drawGrid


  -- * Border
  , BorderF
  , plainBorder
  , noBorder

  ) where

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils
import Graphics.PSC.Utils

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space 



-- How you draw axis labels is quite "shrewd" - i.e 
-- ticks / labels or both, or neither...
--

type AxisF u v = ScaleCtx u v Graphic

type AxisLabelDrawF u = u -> DPoint2 -> Graphic

type AxisSteps u = [u]


xAxisText :: (DRGB,FontAttr) -> Double -> (u -> String) -> AxisLabelDrawF u
xAxisText font_props gap textF = \u north_pt -> 
    textlabelN font_props (textF u) (north_pt .-^ vvec gap)


yAxisText :: (DRGB,FontAttr) -> Double -> (v -> String) -> AxisLabelDrawF v
yAxisText font_props gap textF = \v east_pt -> 
    textlabelE font_props (textF v) (east_pt .-^ hvec gap)



drawAxes :: AxisLabelDrawF u -> AxisSteps u
         -> AxisLabelDrawF v -> AxisSteps v
         -> ScaleCtx u v Graphic
drawAxes udrawF usteps vdrawF vsteps ctx = hf . vf
  where
    hf = horizontalLabels udrawF usteps ctx 
    vf = verticalLabels   vdrawF vsteps ctx
    

horizontalLabels :: AxisLabelDrawF u -> AxisSteps u -> ScaleCtx u v Graphic
horizontalLabels buildF steps = horizontals 0 buildF steps


verticalLabels :: AxisLabelDrawF v -> AxisSteps v -> ScaleCtx u v Graphic
verticalLabels buildF steps = verticals 0 buildF steps 


--------------------------------------------------------------------------------
-- Grids

type GridF u v = ScaleCtx u v Graphic

type StraightLineF = DPoint2 -> DPoint2 -> Graphic


simpleGridLine :: Stroke t => t -> StraightLineF
simpleGridLine t = \p0 p1 -> straightLine t (p1 .-. p0) p0



drawGrid :: StraightLineF -> AxisSteps u -> AxisSteps v -> ScaleCtx u v Graphic
drawGrid drawF usteps vsteps ctx = vf . hf
  where
    hf = horizontalLines drawF vsteps ctx
    vf = verticalLines   drawF usteps ctx


verticalLines :: StraightLineF
              -> AxisSteps u
              -> ScaleCtx u v Graphic
verticalLines drawF steps ctx@(rect,_,_) = horizontals 0 buildF steps ctx
  where
    buildF _ pt  = drawF pt (pt .+^ upvec)
    upvec        = vvec $ rectHeight rect


horizontalLines :: StraightLineF
                -> AxisSteps v
                -> ScaleCtx u v Graphic
horizontalLines drawF steps ctx@(rect,_,_) = verticals 0 buildF steps ctx
  where
    buildF _ pt = drawF pt (pt .+^ rightvec)
    rightvec    = hvec $ rectWidth rect



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
    cmp (_,x) = x `leqEps` rectWidth rect 

yvalues :: AxisSteps v -> ScaleCtx u v [(v,Double)]
yvalues steps (rect,_,fY) = takeWhile cmp $ map (\a -> (a, fY a)) steps
  where
    cmp (_,y) = y `leqEps` rectHeight rect



leqEps :: Double -> Double -> Bool
leqEps a b | a < b     = True
           | otherwise = let diff = a - b in diff < rect_epsilon 

rect_epsilon :: Double 
rect_epsilon = 0.01


--------------------------------------------------------------------------------

type BorderF = DPoint2 -> DPoint2 -> Graphic


plainBorder :: DRGB -> Double -> ScaleCtx u v Graphic
plainBorder rgb lw = \((w,h),_,_) -> 
    strokedRectangle (rgb, LineWidth lw) w h (P2 0 0)

noBorder :: BorderF
noBorder = \ _ _ -> id