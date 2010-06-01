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

module Graphics.PSC.AxisTWO
  where

import Graphics.PSC.Core

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space 



data AxisLabelConfig u v = AxisLabelConfig
      { label_font      :: FontAttr
      , font_colour     :: DRGB
      , x_axis_alg      :: Maybe (AxisLabelAlg u)
      , y_axis_alg      :: Maybe (AxisLabelAlg v)
      } 

data AxisLabelAlg unit = AxisLabelAlg
      { start_value     :: unit
      , step_fun        :: unit -> unit
      , render_fun      :: unit -> String
      }

{-

fontProps :: AxisLabelConfig u v -> (DRGB,FontAttr)
fontProps (AxisLabelConfig {font_colour,label_font}) = (font_colour,label_font)

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
      { grid_line       :: LineConfiguration
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
           Just alg -> hlines fX grid_line alg rect

    ys = case grid_y_axis of 
           Nothing  -> []
           Just alg -> vlines fY grid_line alg rect



hlines :: (v -> Double)
       -> LineConfiguration 
       -> AxisLabelAlg v 
       -> DrawingRectangle
       -> [DPrimitive]
hlines fY line_cfg axis_alg (DrawingRectangle {rect_width,rect_height}) = 
    map (\pt -> sf $ path pt [lineTo $ pt .+^ hvec rect_width]) points
  where
    points  = map (\y -> P2 0 y) $ yvalues fY axis_alg rect_height
    sf      = ostroke (makeStrokeProps line_cfg)


vlines :: (u -> Double) 
       -> LineConfiguration 
       -> AxisLabelAlg u 
       -> DrawingRectangle
       -> [DPrimitive]
vlines fX line_cfg axis_alg (DrawingRectangle {rect_width,rect_height}) = 
    map (\pt -> sf $ path pt [lineTo $ pt .+^ vvec rect_height]) points
  where
    points  = map (\x -> P2 x 0) $ xvalues fX axis_alg rect_width
    sf      = ostroke (makeStrokeProps line_cfg)


xvalues :: (u -> Double) ->  AxisLabelAlg u -> Double -> [Double]
xvalues fX (AxisLabelAlg {start_value,step_fun}) width = 
  takeWhile (< width) $ map fX $ iterate step_fun start_value


yvalues :: (v -> Double) ->  AxisLabelAlg v -> Double -> [Double]
yvalues fY (AxisLabelAlg {start_value,step_fun}) height = 
  takeWhile (< height) $ map fY $ iterate step_fun start_value

