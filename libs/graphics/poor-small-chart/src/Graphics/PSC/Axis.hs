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
-- Spark line
--
--------------------------------------------------------------------------------

module Graphics.PSC.Axis
  where

import Graphics.PSC.RenderMonad

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space 

import Numeric


data AxisLabel u = AxisLabel
      { label_font      :: FontAttr
      , font_colour     :: DRGB
      , start_value     :: u
      , step_count      :: Int
      , step_fun        :: u -> u
      , render_fun      :: u -> String
      }


xaxis :: DRGB -> Int -> Double -> DPoint2 -> RenderM u v [DPrimitive]
xaxis rgb n width start_pt = return [baseline]
  where
    tot_width = width * fromIntegral n
    baseline  = ostroke rgb $ vertexPath [start_pt , start_pt .+^ hvec tot_width]

xlabels :: AxisLabel u -> RenderM u v [DPrimitive]
xlabels attr@(AxisLabel {label_font,font_colour,render_fun}) = 
    mapM (xlabel1 font_colour label_font render_fun) $ 
        take (step_count attr) $ iterate (step_fun attr) (start_value attr)

xlabel1 :: DRGB -> FontAttr -> (u -> String) -> u -> RenderM u v DPrimitive
xlabel1 rgb font_attr toString val = 
    origin        >>= \(P2 _ y) -> 
    scaleX val    >>= \x        ->
    return $ textlabel (rgb,font_attr) (toString val) (P2 x (y-2 * f_height))
  where
    f_height = textHeight $ font_size $ font_attr


ylabels :: AxisLabel v -> RenderM u v [DPrimitive]
ylabels attr@(AxisLabel {label_font,font_colour,render_fun}) = 
    mapM (ylabel1 font_colour label_font render_fun) $ 
        take (step_count attr) $ iterate (step_fun attr) (start_value attr)

ylabel1 :: DRGB -> FontAttr -> (v -> String) -> v -> RenderM u v DPrimitive
ylabel1 rgb font_attr toString val = 
    origin        >>= \(P2 x _) -> 
    scaleY val    >>= \y        ->
    return $ textlabel (rgb,font_attr) label_text (P2 (x - f_width) y)
  where
    label_text = toString val
    f_width = textWidth (font_size $ font_attr) (length label_text)


ffloat :: RealFloat u => Int -> u -> String
ffloat prec = ($ "") . showFFloat (Just prec)

