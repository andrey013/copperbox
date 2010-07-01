{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Legend
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Legends
--
--------------------------------------------------------------------------------

module Graphics.PSC.Legend
  ( 
    LegendElementDrawF
  , ColourLegend
  , drawLegend
  , simpleLegendElementDraw

  ) where

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space 

type LegendElementDrawF = DRGB -> String -> DPoint2 -> Graphic

type ColourLegend = [(DRGB,String)]

drawLegend :: LegendElementDrawF -> Double -> ColourLegend -> Graphic
drawLegend drawF height xs = 
    foldr (.) id $ zipWith (\(rgb,text) pt -> drawF rgb text pt) xs points
  where
    points          = iterate (.-^ vvec (height + 4)) (P2 4 4)

simpleLegendElementDraw :: DRGB -> FontAttr -> LegendElementDrawF
simpleLegendElementDraw text_rgb font_props = 
    \rgb text pt -> let height  = capHeight $ font_size font_props
                        square  = filledRectangle rgb height height pt
                        pt2     = pt .+^ hvec (height + 4)
                        label   = wrapG $ textlabel (text_rgb,font_props) text pt2
                    in square . label 
    

longestName :: ColourLegend -> String
longestName = snd . foldr fn (0,"") where
    fn (_,s) (n,a) = let len = length s in
                     if len > n then (len,s) else (n,a)
