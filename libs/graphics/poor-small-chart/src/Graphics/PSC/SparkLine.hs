{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.SparkLineTWO
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

module Graphics.PSC.SparkLine
  (
  -- * Data types
    SparkLine(..)
  , SparkLineConfig(..)
  , SparkLineF
  , RangeBandF
  
  -- * Draw
  , simpleLine
  , rangeBand
  , noRangeBand
  , renderSparkLine  

  ) where

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils

import Wumpus.Core                      -- package: wumpus-core

import Data.Maybe

     
data SparkLine u v = SparkLine
      { sparkline_config  :: SparkLineConfig
      , sparkline_projs   :: XYProjection u v
      , sparkline_draw    :: SparkLineF
      , range_band        :: RangeBandF v
      }

data SparkLineConfig = SparkLineConfig
      { font_height     :: PointSize
      , letter_count    :: Int
      }




type SparkLineF = [DPoint2] -> Graphic

simpleLine :: DRGB -> Double -> SparkLineF
simpleLine rgb lw = wrapG . ostroke (rgb, LineWidth lw) . vertexPath



renderSparkLine :: SparkLine u v -> Dataset u v -> Chart
renderSparkLine (SparkLine c (px,py) drawF rangeF) ds = 
    fromMaybe errK $ drawGraphic $ drawF points . bkgrnd
  where
    errK   = error "renderSparkLine - empty drawing"
    points = map (\(u,v) -> P2 (fX u) (fY v)) ds

    bkgrnd = rangeF fY w 0

    (w,_)  = pictureSize c

    fX     = makeProjector px
    fY     = makeProjector py


pictureSize :: SparkLineConfig -> (Double,Double)
pictureSize (SparkLineConfig {font_height,letter_count}) =
  (textWidth font_height letter_count, fromIntegral font_height)  


--------------------------------------------------------------------------------
-- Range band

-- The type of RangeBandF is very specifically tailored to 
-- work with the implementation @rangeBand@.

type RangeBandF v = (v -> Double) -> SparkLineWidth -> Double -> Graphic

type SparkLineWidth = Double

rangeBand :: Range v -> DRGB -> RangeBandF v
rangeBand (y0 ::: y1) rgb = 
    \fY w x0 -> let (dy0,dy1) = (fY y0, fY y1)
                    pt        = P2 x0 dy0 in 
                filledRectangle rgb w (dy1-dy0) pt


noRangeBand :: RangeBandF v
noRangeBand = \_ _ _ -> id  



--------------------------------------------------------------------------------
-- Specific dots

-- type DotF = DPoint2 -> Graphic


-- Start and end dots - need to see the dataset
-- Min and max dots   - need to see the dataset



