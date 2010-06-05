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
  , RangeBand

  -- * Draw
  , simpleLine
  , renderSparkLine  

  ) where

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils
import Graphics.PSC.Utils

import Wumpus.Core                      -- package: wumpus-core

import Data.Maybe

     
data SparkLine u v = SparkLine
      { sparkline_config  :: SparkLineConfig v
      , sparkline_projs   :: XYProjection u v
      , sparkline_draw    :: SparkLineF
      }

data SparkLineConfig v = SparkLineConfig
      { font_height     :: PointSize
      , letter_count    :: Int
      , opt_range_band  :: Maybe (RangeBand v)
      }


type RangeBand yu = (DRGB, Range yu) 


type SparkLineF = [DPoint2] -> HPrim Double

simpleLine :: DRGB -> Double -> SparkLineF
simpleLine rgb lw = wrapH . ostroke (rgb, LineWidth lw) . vertexPath



renderSparkLine :: SparkLine u v -> Dataset u v -> Chart
renderSparkLine (SparkLine c (px,py) drawF) ds = 
    fromMaybe errK $ drawHPrim $ drawF points . bkgrnd
  where
    errK   = error "renderSparkLine - empty drawing"
    points = map (\(u,v) -> P2 (fX u) (fY v)) ds

    bkgrnd :: HPrim Double    
    bkgrnd = maybe id (makeRangeBand fY (w,h)) $ opt_range_band c
    (w,h)  = pictureSize c

    fX     = makeProjector px
    fY     = makeProjector py


-- Is there any benefit to changing the representation of
-- RangeBand?
-- 
-- There is only one style of range band - filled rect of some
-- supplied colour, which enumerates to two drawings: range band
-- or no range band 
--
-- type RangeBandF = HPrim
--


makeRangeBand :: (v -> Double) -> (Double,Double) -> RangeBand v -> HPrim Double
makeRangeBand fY (width,_) (rgb,y0 ::: y1) = wrapH $ fill rgb $ vertexPath [bl,br,ur,ul]
  where
    (ya,yb) = (fY y0,fY y1) 
    bl      = P2 0     ya
    br      = P2 width ya
    ur      = P2 width yb
    ul      = P2 0     yb



pictureSize :: SparkLineConfig v -> (Double,Double)
pictureSize (SparkLineConfig {font_height,letter_count}) =
  (textWidth font_height letter_count, fromIntegral font_height)  







-- Start and end dots - need to see the dataset
-- Min and max dots   - need to see the dataset
-- Range band         - needs to know how wide the spark line is
--                      or the min x and max x (then rescales)

