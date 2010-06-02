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
  where

import Graphics.PSC.Core
import Graphics.PSC.Utils

import Wumpus.Core                      -- package: wumpus-core

-- import Data.AffineSpace                 -- package: vector-space 



data AxisLabelConfig u v = AxisLabelConfig
      { label_font      :: FontAttr
      , font_colour     :: DRGB
      , x_axis_alg      :: Maybe (AxisLabelAlg u, u -> String)
      , y_axis_alg      :: Maybe (AxisLabelAlg v, v -> String)
      } 

data AxisLabelAlg unit = AxisLabelAlg
      { start_value     :: unit
      , step_fun        :: unit -> unit
      }



fontProps :: AxisLabelConfig u v -> (DRGB,FontAttr)
fontProps (AxisLabelConfig {font_colour,label_font}) = (font_colour,label_font)



{-

axisLabels :: AxisLabelConfig u v -> RenderM u v [DPrimitive]
axisLabels cfg = (++) <$> optLabels xlabels (x_axis_alg cfg)
                      <*> optLabels ylabels (y_axis_alg cfg)
  where
    optLabels _ Nothing         = return []
    optLabels f (Just alg)      = f (fontProps cfg) alg

xlabels :: (DRGB,FontAttr) -> AxisLabelAlg u -> RenderM u v [DPrimitive]
xlabels dprops attr@(AxisLabelAlg {render_fun}) = 
    mapM (xlabel1 dprops render_fun) $ 
        take (step_count attr) $ iterate (step_fun attr) (start_value attr)




xlabel1 :: (DRGB,FontAttr) -> (u -> String) -> u -> RenderM u v DPrimitive
xlabel1 (rgb,font_attrs) toString val = 
    origin        >>= \(P2 _ y) -> 
    scaleX val    >>= \x        ->
    return $ textlabel (rgb,font_attrs) (toString val) (P2 x (y-2 * f_height))
  where
    f_height = textHeight $ font_size $ font_attrs



ylabels :: (DRGB,FontAttr) -> AxisLabelAlg v -> RenderM u v [DPrimitive]
ylabels dprops attr@(AxisLabelAlg {render_fun}) = 
    mapM (ylabel1 dprops render_fun) $ 
        take (step_count attr) $ iterate (step_fun attr) (start_value attr)

ylabel1 :: (DRGB,FontAttr) -> (v -> String) -> v -> RenderM u v DPrimitive
ylabel1 (rgb,font_attrs) toString val = 
    origin        >>= \(P2 x _) -> 
    scaleY val    >>= \y        ->
    return $ textlabel (rgb,font_attrs) label_text (P2 (x - f_width) y)
  where
    label_text = toString val
    f_width = textWidth (font_size $ font_attrs) (length label_text)



--------------------------------------------------------------------------------
-- Grids
-}


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
    xs ++ ys
  where
    xs = case grid_x_axis of 
           Nothing  -> []
           Just alg -> verticalLines fX grid_line alg rect

    ys = case grid_y_axis of 
           Nothing  -> []
           Just alg -> horizontalLines fY grid_line alg rect



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
xvalues fX (AxisLabelAlg {start_value,step_fun}) draw_rect = 
    takeWhile cmp $ map fX $ iterate step_fun start_value
  where
    cmp x = x `leqEps` rect_width draw_rect 

yvalues :: (v -> Double) ->  AxisLabelAlg v -> DrawingRectangle -> [Double]
yvalues fY (AxisLabelAlg {start_value,step_fun}) draw_rect = 
    takeWhile cmp $ map fY $ iterate step_fun start_value
  where
    cmp y = y `leqEps` rect_height draw_rect

leqEps :: Double -> Double -> Bool
leqEps a b | a < b     = True
           | otherwise = let diff = a - b in diff < rect_epsilon 

rect_epsilon :: Double 
rect_epsilon = 0.01