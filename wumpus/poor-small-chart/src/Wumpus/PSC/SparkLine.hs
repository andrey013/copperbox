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


import Wumpus.PSC.BasicAdditions
import Wumpus.PSC.Core
import Wumpus.PSC.ScaleRectMonad

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Monads.CoordScaleMonad

import Control.Applicative


     
data SparkLine ux uy u = SparkLine
      { sparkline_ctx     :: ScaleCtx ux uy u
      , sparkline_rect    :: Rectangle u
      , sparkline_draw    :: SparkLineF u
      , range_band        :: RangeBand ux uy u
      }


type SparkLineF u = [Point2 u] -> Graphic u

drawSparkLine :: (SparkLine ux uy u) -> Dataset ux uy -> Graphic u
drawSparkLine (SparkLine ctx rect drawF rangeF) ds = 
    runScaleRectM ctx rect $ do { a <- drawLine drawF ds
                                ; b <- rangeF
                                ; return $ a . b
                                }

simpleLine :: Num u => DRGB -> Double -> SparkLineF u
simpleLine rgb lw = wrapG . ostroke (rgb, LineWidth lw) . vertexPath


drawLine :: SparkLineF u -> Dataset ux uy -> ScaleRectM ux uy u (Graphic u)
drawLine drawF ds = drawF <$> mapM coordScale ds


sparklineRectangle :: Fractional u => FontAttr -> Int -> Rectangle u 
sparklineRectangle attr letter_count = 
    Rectangle (textWidth sz letter_count) (fromIntegral sz)
  where
    sz = font_size attr



--------------------------------------------------------------------------------
-- Range band

-- The type of RangeBandF is very specifically tailored to 
-- work with the implementation @rangeBand@.


type RangeBand ux uy u = ScaleRectM ux uy u (Graphic u)



rangeBand :: (Num ux, Num uy, Num u) => Range uy -> DRGB -> RangeBand ux uy u
rangeBand (y0 ::: y1) rgb = 
       (\w h y -> wrapG $ fill rgb $ rectanglePath w h (P2 0 y))
   <$> borderWidth <*> yScale (y1 - y0) <*> yScale y0   



noRangeBand :: RangeBand ux uy u
noRangeBand = return blankG




--------------------------------------------------------------------------------
-- Specific dots

-- type DotF = DPoint2 -> Graphic


-- Start and end dots - need to see the dataset
-- Min and max dots   - need to see the dataset



