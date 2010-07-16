{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.Axis
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

module Wumpus.PSC.Axis
  ( 
  -- * Axes
    AxisSteps
  , AxisLabelF

  , LabelConfig(..)
  , xAxisLabel
  , yAxisLabel

  , TickLabelConfig(..)
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

import Wumpus.PSC.Core
import Wumpus.PSC.DrawingUtils

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Data.AffineSpace                 -- package: vector-space 



-- How you draw axis labels is quite "shrewd" - i.e 
-- ticks / labels or both, or neither...
--

type AxisLabelF u = u -> DPoint2 -> DGraphic

type AxisSteps u = [u]




data LabelConfig = LabelConfig
      { label_font_colour       :: DRGB
      , label_font_attr         :: FontAttr
      , label_gap_length        :: Double
      }


-- | 'xAxisLabel' 
-- 
xAxisLabel :: LabelConfig -> (u -> String) -> AxisLabelF u
xAxisLabel (LabelConfig frgb fattr gap) textF = \u north_pt -> 
    textlabelN (frgb,fattr) (textF u) (north_pt .-^ vvec gap)


-- | 'yAxisLabel' 
-- 
yAxisLabel :: LabelConfig -> (v -> String) -> AxisLabelF v
yAxisLabel (LabelConfig frgb fattr gap) textF = \v east_pt -> 
    textlabelE (frgb,fattr) (textF v) (east_pt .-^ hvec gap)


data TickLabelConfig = TickLabelConfig
      { tick_label_font_colour      :: DRGB
      , tick_label_font_attr        :: FontAttr
      , tick_label_tick_colour      :: DRGB
      , tick_label_line_width       :: Double
      , tick_label_tick_length      :: Double
      , tick_label_gap_length       :: Double
      }


-- TODO/Note - the placement/geometry could (probably) be 
-- supplied in the Config data type rather than having four 
-- separate functions.
-- 
-- But maybe again a functional representation will have
-- advantages...
--

-- | 'xAxisLabelTick' 
-- 
-- Label at the bottom, tick above it...
--
-- >  |
-- > 1.0
--
xAxisTickLabel :: TickLabelConfig
               -> (v -> String) 
               -> AxisLabelF v
xAxisTickLabel (TickLabelConfig frgb fattr lrgb lw tick_len gap) textF = 
     \v point -> line point . label v point
  where
    label v pt = textlabelN (frgb,fattr) (textF v) (pt .-^ vvec (tick_len + gap))
    line pt    = straightLine (lrgb, LineWidth lw) (vvec (negate tick_len)) pt


-- | 'xAxisLabelTickAlt' 
-- 
-- Tick at the bottom, label above it...
--
-- > 1.0
-- >  |
--
xAxisTickLabelAlt :: TickLabelConfig -> (v -> String) -> AxisLabelF v
xAxisTickLabelAlt (TickLabelConfig frgb fattr lrgb lw tick_len gap) textF = 
     \v point -> line point . label v point
  where
    label v pt = textlabelS (frgb,fattr) (textF v) (pt .+^ vvec (tick_len + gap))
    line pt    = straightLine (lrgb, LineWidth lw) (vvec tick_len) pt


-- | 'yAxisLabelTick' 
-- 
-- Label on the left, tick to its right...
--
-- > 1.0 --
--
yAxisTickLabel :: TickLabelConfig -> (v -> String) -> AxisLabelF v
yAxisTickLabel (TickLabelConfig frgb fattr lrgb lw tick_len gap) textF = 
     \v point -> line point . label v point
  where
    label v pt = textlabelE (frgb,fattr) (textF v) (pt .-^ hvec (tick_len + gap))
    line pt    = straightLine (lrgb, LineWidth lw) (hvec (negate tick_len)) pt

-- | 'yAxisTickLabelAlt' 
-- 
-- Tick on the left, label to its right...
--
-- > -- 1.0
--
yAxisTickLabelAlt :: TickLabelConfig -> (v -> String) -> AxisLabelF v
yAxisTickLabelAlt (TickLabelConfig frgb fattr lrgb lw tick_len gap) textF = 
     \v point -> line point . label v point
  where
    label v pt = textlabelW (frgb,fattr) (textF v) (pt .+^ hvec (tick_len + gap))
    line pt    = straightLine (lrgb, LineWidth lw) (hvec tick_len) pt

    

drawAxes :: AxisLabelF u -> AxisSteps u
         -> AxisLabelF v -> AxisSteps v
         -> ScaleCtx u v DGraphic
drawAxes udrawF usteps vdrawF vsteps = hf `cc` vf
  where
    hf = horizontalLabels udrawF usteps
    vf = verticalLabels   vdrawF vsteps
    

horizontalLabels :: AxisLabelF u -> AxisSteps u -> ScaleCtx u v DGraphic
horizontalLabels buildF steps = horizontals 0 buildF steps


verticalLabels :: AxisLabelF v -> AxisSteps v -> ScaleCtx u v DGraphic
verticalLabels buildF steps = verticals 0 buildF steps 

horizontalLabelsTop :: AxisLabelF u -> AxisSteps u -> ScaleCtx u v DGraphic
horizontalLabelsTop buildF steps = \ctx -> 
    horizontals (ctxRectangleHeight ctx) buildF steps ctx


verticalLabelsRight :: AxisLabelF v -> AxisSteps v -> ScaleCtx u v DGraphic
verticalLabelsRight buildF steps = \ctx -> 
    verticals (ctxRectangleWidth ctx) buildF steps ctx



--------------------------------------------------------------------------------
-- Grids


type GridLineF = DPoint2 -> DPoint2 -> DGraphic


simpleGridLine :: Stroke t => t -> GridLineF
simpleGridLine t = \p0 p1 -> straightLine t (p1 .-. p0) p0



drawGrid :: GridLineF -> AxisSteps u -> AxisSteps v -> ScaleCtx u v DGraphic
drawGrid drawF usteps vsteps ctx = vf . hf
  where
    hf = horizontalLines drawF vsteps ctx
    vf = verticalLines   drawF usteps ctx


verticalLines :: GridLineF
              -> AxisSteps u
              -> ScaleCtx u v DGraphic
verticalLines drawF steps ctx@(rect,_,_) = horizontals 0 buildF steps ctx
  where
    buildF _ pt  = drawF pt (pt .+^ upvec)
    upvec        = vvec $ rectHeight rect


horizontalLines :: GridLineF
                -> AxisSteps v
                -> ScaleCtx u v DGraphic
horizontalLines drawF steps ctx@(rect,_,_) = verticals 0 buildF steps ctx
  where
    buildF _ pt = drawF pt (pt .+^ rightvec)
    rightvec    = hvec $ rectWidth rect



--------------------------------------------------------------------------------
-- Enumerate x-y values...

horizontals :: Double 
            -> (u -> DPoint2 -> DGraphic) 
            -> AxisSteps u 
            -> ScaleCtx u v DGraphic
horizontals y0 buildF steps ctx = 
    veloH (\(xu,x) -> buildF xu (P2 x y0)) $ xvalues steps ctx


verticals :: Double 
          -> (v -> DPoint2 -> DGraphic) 
          -> AxisSteps v 
          -> ScaleCtx u v DGraphic 
verticals x0 buildF steps ctx = 
    veloH (\(yu,y) -> buildF yu (P2 x0 y)) $ yvalues steps ctx



xvalues :: AxisSteps u -> ScaleCtx u v [(u,Double)]
xvalues steps (rect,fX,_) = takeWhile cmp $ map (\a -> (a,fX a)) steps
  where
    cmp (_,a) = a `leqEps` rectWidth rect 

yvalues :: AxisSteps v -> ScaleCtx u v [(v,Double)]
yvalues steps (rect,_,fY) = takeWhile cmp $ map (\a -> (a, fY a)) steps
  where
    cmp (_,a) = a `leqEps` rectHeight rect



leqEps :: Double -> Double -> Bool
leqEps a b | a < b     = True
           | otherwise = let diff = a - b in diff < rect_epsilon 

rect_epsilon :: Double 
rect_epsilon = 0.01


--------------------------------------------------------------------------------

type BorderF = DPoint2 -> DPoint2 -> DGraphic


plainBorder :: DRGB -> Double -> ScaleCtx u v DGraphic
plainBorder rgb lw = \((w,h),_,_) -> 
    strokedRectangle (rgb, LineWidth lw) w h (P2 0 0)

noBorder :: BorderF
noBorder = \ _ _ -> id