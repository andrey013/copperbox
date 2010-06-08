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
    AxisSteps
  , AxisLabelF
 
  , xAxisLabel
  , yAxisLabel

  , xAxisTickLabel
  , xAxisTickLabelAlt

  , yAxisTickLabel
  , yAxisTickLabelAlt

  , drawAxes
  , horizontalLabels
  , verticalLabels
  , horizontalLabelsTop
  , verticalLabelsRight

  -- * Grids
  , GridLineF
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

type AxisLabelF u = u -> DPoint2 -> Graphic

type AxisSteps u = [u]

-- | 'xAxisLabel' 
-- : @ (rgb,font_attr) * vertical_gap * to_string -> label_functional @
-- 
xAxisLabel :: (DRGB,FontAttr) -> Double -> (u -> String) -> AxisLabelF u
xAxisLabel font_props gap textF = \u north_pt -> 
    textlabelN font_props (textF u) (north_pt .-^ vvec gap)


-- | 'yAxisLabel' 
-- : @ (rgb,font_attr) * horizontal_gap * to_string -> label_functional @
-- 
yAxisLabel :: (DRGB,FontAttr) -> Double -> (v -> String) -> AxisLabelF v
yAxisLabel font_props gap textF = \v east_pt -> 
    textlabelE font_props (textF v) (east_pt .-^ hvec gap)


-- | 'xAxisLabelTick' 
-- : @ (tick_rgb, tick_line_width) * (font_rgb,font_attr) 
--   * tick_length * vertical_gap  * to_string -> label_functional @
-- 
-- Label at the bottom, tick above it...
--
-- >  |
-- > 1.0
--
xAxisTickLabel :: (DRGB,Double) 
               -> (DRGB,FontAttr) 
               -> Double -> Double 
               -> (v -> String) 
               -> AxisLabelF v
xAxisTickLabel (rgb,lw) font_props tick_len gap textF = 
     \v point -> line point . label v point
  where
    label v pt = textlabelN font_props (textF v) (pt .-^ vvec (tick_len + gap))
    line pt    = straightLine (rgb, LineWidth lw) (vvec (negate tick_len)) pt


-- | 'xAxisLabelTick' 
-- : @ (tick_rgb, tick_line_width) * (font_rgb,font_attr) 
--   * tick_length * vertical_gap  * to_string -> label_functional @
-- 
-- Tick at the bottom, label above it...
--
-- > 1.0
-- >  |
--
xAxisTickLabelAlt :: (DRGB,Double) 
                  -> (DRGB,FontAttr) 
                  -> Double -> Double 
                  -> (v -> String) 
                  -> AxisLabelF v
xAxisTickLabelAlt (rgb,lw) font_props tick_len gap textF = 
     \v point -> line point . label v point
  where
    label v pt = textlabelS font_props (textF v) (pt .+^ vvec (tick_len + gap))
    line pt    = straightLine (rgb, LineWidth lw) (vvec tick_len) pt


-- | 'yAxisLabelTick' 
-- : @ (tick_rgb, tick_line_width) * (font_rgb,font_attr) 
--   * tick_length * horizontal_gap  * to_string -> label_functional @
-- 
-- Label on the left, tick to its right...
--
-- > 1.0 --
--
yAxisTickLabel :: (DRGB,Double) 
               -> (DRGB,FontAttr) 
               -> Double -> Double 
               -> (v -> String) 
               -> AxisLabelF v
yAxisTickLabel (rgb,lw) font_props tick_len gap textF = 
     \v point -> line point . label v point
  where
    label v pt = textlabelE font_props (textF v) (pt .-^ hvec (tick_len + gap))
    line pt    = straightLine (rgb, LineWidth lw) (hvec (negate tick_len)) pt

-- | 'yAxisTickLabelAlt' 
-- : @ (tick_rgb, tick_line_width) * (font_rgb,font_attr) 
--   * tick_length * horizontal_gap  * to_string -> label_functional @ 
-- 
-- Tick on the left, label to its right...
--
-- > -- 1.0
--
yAxisTickLabelAlt :: (DRGB,Double) 
                  -> (DRGB,FontAttr) 
                  -> Double -> Double 
                  -> (v -> String) 
                  -> AxisLabelF v
yAxisTickLabelAlt (rgb,lw) font_props tick_len gap textF = 
     \v point -> line point . label v point
  where
    label v pt = textlabelW font_props (textF v) (pt .+^ hvec (tick_len + gap))
    line pt    = straightLine (rgb, LineWidth lw) (hvec tick_len) pt

    

drawAxes :: AxisLabelF u -> AxisSteps u
         -> AxisLabelF v -> AxisSteps v
         -> ScaleCtx u v Graphic
drawAxes udrawF usteps vdrawF vsteps = hf `cc` vf
  where
    hf = horizontalLabels udrawF usteps
    vf = verticalLabels   vdrawF vsteps
    

horizontalLabels :: AxisLabelF u -> AxisSteps u -> ScaleCtx u v Graphic
horizontalLabels buildF steps = horizontals 0 buildF steps


verticalLabels :: AxisLabelF v -> AxisSteps v -> ScaleCtx u v Graphic
verticalLabels buildF steps = verticals 0 buildF steps 

horizontalLabelsTop :: AxisLabelF u -> AxisSteps u -> ScaleCtx u v Graphic
horizontalLabelsTop buildF steps = \ctx -> 
    horizontals (ctxRectangleHeight ctx) buildF steps ctx


verticalLabelsRight :: AxisLabelF v -> AxisSteps v -> ScaleCtx u v Graphic
verticalLabelsRight buildF steps = \ctx -> 
    verticals (ctxRectangleWidth ctx) buildF steps ctx



--------------------------------------------------------------------------------
-- Grids


type GridLineF = DPoint2 -> DPoint2 -> Graphic


simpleGridLine :: Stroke t => t -> GridLineF
simpleGridLine t = \p0 p1 -> straightLine t (p1 .-. p0) p0



drawGrid :: GridLineF -> AxisSteps u -> AxisSteps v -> ScaleCtx u v Graphic
drawGrid drawF usteps vsteps ctx = vf . hf
  where
    hf = horizontalLines drawF vsteps ctx
    vf = verticalLines   drawF usteps ctx


verticalLines :: GridLineF
              -> AxisSteps u
              -> ScaleCtx u v Graphic
verticalLines drawF steps ctx@(rect,_,_) = horizontals 0 buildF steps ctx
  where
    buildF _ pt  = drawF pt (pt .+^ upvec)
    upvec        = vvec $ rectHeight rect


horizontalLines :: GridLineF
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