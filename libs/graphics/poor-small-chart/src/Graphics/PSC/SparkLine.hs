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
  , RangeBand

  -- * Draw
  , renderSparkLine  

  ) where

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils

import Wumpus.Core                      -- package: wumpus-core

import Data.Maybe

     
data SparkLine u v = SparkLine
      { sparkline_config  :: SparkLineConfig v
      , sparkline_projs   :: XYProjection u v
      , sparkline_style   :: LineConfig
      , sparkline_data    :: Dataset u v
      }

data SparkLineConfig v = SparkLineConfig
      { font_height     :: PointSize
      , letter_count    :: Int
      , opt_range_band  :: Maybe (RangeBand v)
      }


type RangeBand yu = (DRGB, yu, yu) 


renderSparkLine :: SparkLine u v -> Chart
renderSparkLine (SparkLine c (px,py) props ds) = 
    frameMulti $ catMaybes [ Just sline, bkgrnd ]
  where
    sline  = ostroke (makeStrokeProps props) $ vertexPath points
    points = map (\(u,v) -> P2 (fX u) (fY v)) ds
    

    bkgrnd = fmap (makeRangeBand fY (w,h)) $ opt_range_band c
    (w,h)  = pictureSize c

    fX     = makeProjector px
    fY     = makeProjector py


makeRangeBand :: (v -> Double) -> (Double,Double) -> RangeBand v -> DPrimitive
makeRangeBand fY (width,_) (rgb,y0,y1) = fill rgb $ vertexPath [bl,br,ur,ul]
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


-- It\'s unsafe to take the scaling directly from the data. 
-- 
-- Do this meanas that you can\'t satck one spark line on 
-- another -- this is the problem the initial scatter plot had.

