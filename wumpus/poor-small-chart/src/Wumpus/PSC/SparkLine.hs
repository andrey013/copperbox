{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.SparkLine
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Spark lines
--
--------------------------------------------------------------------------------

module Wumpus.PSC.SparkLine
  where


import Wumpus.PSC.Bivariate
import Wumpus.PSC.BivariateGraphic
import Wumpus.PSC.Core

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic



type SparkLineF = [DPoint2] -> DGraphic


simpleLine :: DRGB -> Double -> SparkLineF
simpleLine rgb lw = wrapG . ostroke (rgb, LineWidth lw) . vertexPath


sparkLine :: SparkLineF -> Dataset ux uy -> BivariateGraphic ux uy
sparkLine drawF ds = \bv -> drawF $ map (scaleXY `flip` bv) ds


sparklineRectangle :: FontAttr -> Int -> DRectangle
sparklineRectangle attr letter_count = 
    Rectangle (fromPtSize $ textWidth sz letter_count) (fromIntegral sz)
  where
    sz = font_size attr



--------------------------------------------------------------------------------
-- Range band

-- The type of RangeBandF is very specifically tailored to 
-- work with the implementation @rangeBand@.




rangeBand :: (Num ux, Num uy) => Range uy -> DRGB -> BivariateGraphic ux uy
rangeBand (y0 ::: y1) rgb = \bv -> 
    draw (borderWidth bv) (scaleY (y1 - y0) bv) (bx bv) (scaleY y0 bv) 
  where
    draw w h x y = wrapG $ fill rgb $ rectanglePath w h (P2 x y)
    bx bv = let (P2 x _) = borderOrigin bv in x



noRangeBand :: BivariateGraphic ux uy
noRangeBand = const emptyG




--------------------------------------------------------------------------------
-- Specific dots

-- type DotF = DPoint2 -> Graphic


-- Start and end dots - need to see the dataset
-- Min and max dots   - need to see the dataset



