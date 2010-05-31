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

module Graphics.PSC.SparkLineTWO
  where

import Graphics.PSC.Core

import Wumpus.Core                      -- package: wumpus-core

import Data.Maybe



type Chart = DPicture

writeChartEPS :: FilePath -> Chart -> IO ()
writeChartEPS = writeEPS_latin1 


writeChartSVG :: FilePath -> Chart -> IO ()
writeChartSVG = writeSVG_latin1 



type Dataset u v = [(u,v)]
     

data Projection u = Projection 
      { proj_conv   :: u -> Double
      , proj_trans  :: Double
      , proj_scale  :: Double
      }

data DashLine = DashLine Int [(Int,Int)] 

data LineConfiguration = LineConfiguration
      { line_colour       :: DRGB
      , line_width        :: Double
      , opt_dash_pattern  :: Maybe DashLine
      }



type RangeBand yu = (DRGB, yu, yu) 

data SparkLineConfiguration v = SparkLineConfiguration
      { font_height     :: PointSize
      , letter_count    :: Int
      , opt_range_band  :: Maybe (RangeBand v)
      }

data SparkLine u v = SparkLine
      { sparkline_config  :: SparkLineConfiguration v
      , sparkline_projs   :: (Projection u, Projection v)
      , sparkline_style   :: LineConfiguration
      , sparkline_data    :: Dataset u v
      }

compileSparkLine :: SparkLine u v -> Chart
compileSparkLine (SparkLine c (px,py) props ds) = 
    frameMulti $ catMaybes [ Just sline, bkgrnd ]
  where
    sline  = ostroke (makeStrokeProps props) $ vertexPath points
    points = map (\(u,v) -> P2 (tX u) (tY v)) ds
    

    bkgrnd = fmap (makeRangeBand tY (w,h)) $ opt_range_band c
    (w,h)  = pictureSize c

    tX     = makeProjector px
    tY     = makeProjector py

makeStrokeProps :: LineConfiguration -> (DRGB,[StrokeAttr])
makeStrokeProps (LineConfiguration rgb lw mb_dash) = 
    (rgb, catMaybes [ Just $ LineWidth lw, fmap mkDash mb_dash] )
  where
    mkDash (DashLine offset xs) = DashPattern $ Dash offset xs

makeRangeBand :: (v -> Double) -> (Double,Double) -> RangeBand v -> DPrimitive
makeRangeBand fn (width,_) (rgb,y0,y1) = fill rgb $ vertexPath [bl,br,ur,ul]
  where
    (ya,yb) = (fn y0,fn y1) 
    bl      = P2 0     ya
    br      = P2 width ya
    ur      = P2 width yb
    ul      = P2 0     yb


makeProjector :: Projection u -> (u -> Double)
makeProjector (Projection {proj_conv,proj_trans,proj_scale}) = 
    \u -> ((proj_conv u) - proj_trans) * proj_scale



pictureSize :: SparkLineConfiguration v -> (Double,Double)
pictureSize (SparkLineConfiguration {font_height,letter_count}) =
  (textWidth font_height letter_count, fromIntegral font_height)  


-- Start and end dots - need to see the dataset
-- Min and max dots   - need to see the dataset
-- Range band         - needs to know how wide the spark line is
--                      or the min x and max x (then rescales)


-- It\'s unsafe to take the scaling directly from the data. 
-- 
-- Do this meanas that you can\'t satck one spark line on 
-- another -- this is the problem the initial scatter plot had.


minMax :: (Ord u, Ord v) => Dataset u v -> ((u,v), (u,v))
minMax (x:xs) = foldr fn (x,x) xs where
    fn (u,v) ((u0,v0),(u1,v1)) = ((min u u0, min v v0), (max u u1, max v v1))
minMax _                    = error $ "minMax - empty dataset."


