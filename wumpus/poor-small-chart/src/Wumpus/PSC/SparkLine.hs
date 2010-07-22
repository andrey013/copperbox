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


import Wumpus.PSC.Core
import Wumpus.PSC.ScaleRectMonad

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Monads.CoordScaleMonad

import Control.Applicative


     
data SparkLine ux uy = SparkLine
      { sparkline_ctx     :: ScaleCtx ux uy Double
      , sparkline_rect    :: DRectangleLoc
      , sparkline_draw    :: SparkLineF
      , range_band        :: RangeBand ux uy
      }


type SparkLineF = [DPoint2] -> DGraphic

drawSparkLine :: (SparkLine ux uy) -> Dataset ux uy -> DGraphic
drawSparkLine (SparkLine ctx rect drawF rangeF) ds = 
    runScaleRectM ctx rect $ do { a <- drawLine drawF ds
                                ; b <- rangeF
                                ; return $ a . b
                                }

simpleLine :: DRGB -> Double -> SparkLineF
simpleLine rgb lw = wrapG . ostroke (rgb, LineWidth lw) . vertexPath


drawLine :: SparkLineF -> Dataset ux uy -> ScaleRectM ux uy DGraphic
drawLine drawF ds = drawF <$> mapM coordScale ds


sparklineRectangle :: FontAttr -> Int -> DRectangle
sparklineRectangle attr letter_count = 
    Rectangle (textWidth sz letter_count) (fromIntegral sz)
  where
    sz = font_size attr



--------------------------------------------------------------------------------
-- Range band

-- The type of RangeBandF is very specifically tailored to 
-- work with the implementation @rangeBand@.


type RangeBand ux uy = ScaleRectM ux uy DGraphic



rangeBand :: (Num ux, Num uy) => Range uy -> DRGB -> RangeBand ux uy
rangeBand (y0 ::: y1) rgb = 
       (\w h y -> wrapG $ fill rgb $ rectanglePath w h (P2 0 y))
   <$> borderWidth <*> yScale (y1 - y0) <*> yScale y0   



noRangeBand :: RangeBand ux uy
noRangeBand = return blankG




--------------------------------------------------------------------------------
-- Specific dots

-- type DotF = DPoint2 -> Graphic


-- Start and end dots - need to see the dataset
-- Min and max dots   - need to see the dataset



