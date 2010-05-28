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

xlabels :: Num v => AxisLabel u -> RenderM u v [DPrimitive]
xlabels attr@(AxisLabel {label_font,render_fun}) = 
    mapM (xlabel1 label_font render_fun) $ 
        take (step_count attr) $ iterate (step_fun attr) (start_value attr)


-- NOTE - (val,0) - should not be 0, but the minimum value from
-- the y_range of the plot...
--
-- Maybe Geom needs extending to supply access to this value.
--
xlabel1 :: Num v => FontAttr -> (u -> String) -> u -> RenderM u v DPrimitive
xlabel1 font_attr toString val = scalePoint (val,0) >>= \ pt ->
              return $ textlabel font_attr (toString val) pt


ffloat :: RealFloat u => Int -> u -> String
ffloat prec = ($ "") . showFFloat (Just prec)

