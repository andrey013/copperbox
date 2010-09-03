{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.Legend
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

module Wumpus.PSC.Legend
  ( 
    LegendElementDrawF
  , ColourLegend
  , drawLegend
  , simpleLegendElementDraw

  ) where

import Wumpus.PSC.Core

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic


import Data.AffineSpace                 -- package: vector-space 

type LegendElementDrawF u = RGBi -> String -> DPoint2 -> Graphic u

type ColourLegend = [(RGBi,String)]

drawLegend :: LegendElementDrawF u -> Double -> ColourLegend -> Graphic u
drawLegend drawF height xs = 
    foldr (.) id $ zipWith (\(rgb,text) pt -> drawF rgb text pt) xs points
  where
    points          = iterate (.-^ vvec (height + 4)) (P2 4 4)

simpleLegendElementDraw :: RGBi -> FontAttr -> LegendElementDrawF Double
simpleLegendElementDraw text_rgb font_props = \rgb text pt -> 
    let height  = fromPtSize $ numeralHeight $ font_size font_props
        square  = filledRectangle rgb height height pt
        pt2     = pt .+^ hvec (height + 4)
        label   = wrapG $ textlabel (text_rgb,font_props) text pt2
    in square . label 
    

longestName :: ColourLegend -> String
longestName = snd . foldr fn (0,"") where
    fn (_,s) (n,a) = let len = length s in
                     if len > n then (len,s) else (n,a)
