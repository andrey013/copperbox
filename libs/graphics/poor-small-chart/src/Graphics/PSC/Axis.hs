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
    AxisLabelConfig(..)
  , AxisLabelAlg(..)
  , axisLabels

  , GridConfig(..)
  , hlines
  , vlines

  ) where

import Graphics.PSC.RenderMonad

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space 

import Control.Applicative


data AxisLabelConfig u v = AxisLabelConfig
      { label_font      :: FontAttr
      , font_colour     :: DRGB
      , x_axis_alg      :: Maybe (AxisLabelAlg u)
      , y_axis_alg      :: Maybe (AxisLabelAlg v)
      } 

data AxisLabelAlg unit = AxisLabelAlg
      { start_value     :: unit
      , step_count      :: Int
      , step_fun        :: unit -> unit
      , render_fun      :: unit -> String
      }

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

data GridConfig = GridConfig
      { line_colour     :: DRGB 
      , line_width      :: Double
      }




vlines :: GridConfig -> AxisLabelAlg u -> RenderM u v [DPrimitive]
vlines grid_cfg (AxisLabelAlg {start_value,step_fun}) = 
    yAxisRange                                    >>= \(v,_) ->
    generatePoints (start_value,v) (step_fun, id) >>= \ps      ->
    verticalBounds                                >>= \(y0,y1) ->
    return $ map (\pt -> sf $ path pt [lineTo $ pt .+^ vvec (y1-y0)]) ps
  where
    sf = ostroke (line_colour grid_cfg, [LineWidth $ line_width grid_cfg])



hlines :: GridConfig -> AxisLabelAlg v -> RenderM u v [DPrimitive]
hlines grid_cfg (AxisLabelAlg {start_value,step_fun}) = 
    xAxisRange                                    >>= \(u,_) ->
    generatePoints (u,start_value) (id, step_fun) >>= \ps      ->
    horizontalBounds                              >>= \(x0,x1) ->
    return $ map (\pt -> sf $ path pt [lineTo $ pt .+^ hvec (x1-x0)]) ps
  where
    sf = ostroke (line_colour grid_cfg, [LineWidth $ line_width grid_cfg])

