{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Legend
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

module Graphics.PSC.Legend
  where

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space 



data LegendConfig = LegendConfig
      { legend_label_cfg      :: LabelConfig
      , legend_borderline     :: Maybe LineConfig
      } 


type ColourLegend = [(DRGB,String)]

drawLegend :: LegendConfig -> ColourLegend -> DPicture
drawLegend (LegendConfig {legend_label_cfg}) xs = 
    frameMulti $ foldr fn [] $ zipWith (legend1 legend_label_cfg) xs points
  where
    height          = textHeight $ font_size $ label_font legend_label_cfg
    points          = iterate (.-^ vvec (height + 4)) (P2 4 4)
    fn (p1,p2) acc  = p1:p2:acc

legend1 :: LabelConfig -> (DRGB,String) -> DPoint2 -> (DPrimitive,DPrimitive)
legend1 (LabelConfig {label_text_colour,label_font}) (rgb,text) pt = 
    (square,label)
  where
    height      = textHeight $ font_size label_font
    square      = fill rgb $ vertexPath $ rectPoints height height pt
    pt2         = pt .+^ hvec (height + 4)
    label       = textlabel (label_text_colour,label_font) text pt2


longestName :: ColourLegend -> String
longestName = snd . foldr fn (0,"") where
    fn (_,s) (n,a) = let len = length s in
                     if len > n then (len,s) else (n,a)
