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
import Graphics.PSC.Utils

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




type SparkLineF = [DPoint2] -> HPrim Double

simpleLine :: DRGB -> Double -> SparkLineF
simpleLine rgb lw = wrapH . ostroke (rgb, LineWidth lw) . vertexPath



renderSparkLine :: SparkLine u v -> Dataset u v -> Chart
renderSparkLine (SparkLine c (px,py) drawF rangeF) ds = 
    fromMaybe errK $ drawHPrim $ drawF points . bkgrnd
  where
    errK   = error "renderSparkLine - empty drawing"
    points = map (\(u,v) -> P2 (fX u) (fY v)) ds

    bkgrnd :: HPrim Double    
    bkgrnd = rangeF fY w 0

    (w,_)  = pictureSize c

    fX     = makeProjector px
    fY     = makeProjector py

-- The type of RangeBandF is very specially tailored to 
-- work with the implementation @rangeBand@.

type RangeBandF v = (v -> Double) -> SparkLineWidth -> Double -> HPrim Double

type SparkLineWidth = Double

rangeBand :: Range v -> DRGB -> RangeBandF v
rangeBand (y0 ::: y1) rgb = 
    \fY w x0 -> let (dy0,dy1) = (fY y0, fY y1)
                    pt        = P2 x0 dy0 in 
                wrapH $ fill rgb $ vertexPath $ rectPoints w (dy1-dy0) pt


noRangeBand :: RangeBandF v
noRangeBand = \_ _ _ -> id  


pictureSize :: SparkLineConfig -> (Double,Double)
pictureSize (SparkLineConfig {font_height,letter_count}) =
  (textWidth font_height letter_count, fromIntegral font_height)  



-- Start and end dots - need to see the dataset
-- Min and max dots   - need to see the dataset
-- Range band         - needs to know how wide the spark line is
--                      or the min x and max x (then rescales)

