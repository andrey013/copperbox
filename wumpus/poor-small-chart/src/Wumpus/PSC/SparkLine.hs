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
  (
  -- * Data types
    SparkLine(..)
  , SparkLineF
  , RangeBandF
  
  -- * Draw
  , simpleLine
  , rangeBand
  , noRangeBand
  , renderSparkLine  
  , sparklineRectangle

  ) where

import Wumpus.PSC.Core

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic

import Data.Maybe

     
data SparkLine u v = SparkLine
      { sparkline_ctx     :: DrawingContext u v
      , sparkline_draw    :: SparkLineF
      , range_band        :: RangeBandF u v
      }


type SparkLineF = [DPoint2] -> DGraphic

simpleLine :: DRGB -> Double -> SparkLineF
simpleLine rgb lw = wrapG . ostroke (rgb, LineWidth lw) . vertexPath



renderSparkLine :: SparkLine u v -> Dataset u v -> Chart
renderSparkLine (SparkLine ctx@(_,fX,fY) drawF rangeF) ds = 
    fromMaybe errK $ drawGraphic $ drawF points . bkgrnd
  where
    errK   = error "renderSparkLine - empty drawing"
    points = map (\(u,v) -> P2 (fX u) (fY v)) ds

    bkgrnd = rangeF ctx


sparklineRectangle :: FontAttr -> Int -> DrawingRectangle
sparklineRectangle attr letter_count = 
    (textWidth sz letter_count, fromIntegral sz)
  where
    sz = font_size attr


--------------------------------------------------------------------------------
-- Range band

-- The type of RangeBandF is very specifically tailored to 
-- work with the implementation @rangeBand@.


type RangeBandF u v = ScaleCtx u v DGraphic


rangeBand :: Num v => Range v -> DRGB -> RangeBandF u v
rangeBand (y0 ::: y1) rgb = \(rect,_,fY) -> 
    wrapG $ fill rgb $ rectanglePath (rectWidth rect) (fY $ y1 - y0) (P2 0 (fY y0))



noRangeBand :: RangeBandF u v
noRangeBand = \_ -> id  



--------------------------------------------------------------------------------
-- Specific dots

-- type DotF = DPoint2 -> Graphic


-- Start and end dots - need to see the dataset
-- Min and max dots   - need to see the dataset



