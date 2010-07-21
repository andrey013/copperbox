{-# OPTIONS -Wall #-}

module Spark1 where

import Wumpus.PSC.Core
-- import Wumpus.PSC.DrawingUtils
import Wumpus.PSC.SparkLine

import Wumpus.Core

import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Monads.CoordScaleMonad
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours 

import Data.Maybe
import System.Directory


main :: IO ()
main =  createDirectoryIfMissing True "./out/"
     >> writeChartEPS "./out/spark1.eps" chart1
     >> writeChartSVG "./out/spark1.svg" chart1

chart1 :: Chart Double
chart1 = fromMaybe errK $ drawGraphic $ drawSparkLine spark_cfg data_points

errK :: a
errK = error "empty Graphic"

spark_cfg :: SparkLine Double Double Double
spark_cfg = SparkLine spark_ctx 
                      output_rect
                      spark_stroke
                      spark_rangeband



spark_stroke :: SparkLineF Double
spark_stroke = simpleLine black 0.5


spark_rangeband :: RangeBand Double Double Double
spark_rangeband = rangeBand (0.3 ::: 0.8) aquamarine



spark_ctx :: ScaleCtx Double Double Double
spark_ctx = rectangleScaleCtx ((0.1 ::: 1.0), id) ((0.0 ::: 1.0), id) output_rect



output_rect :: Rectangle Double
output_rect = sparklineRectangle spark_font 10

spark_font :: FontAttr
spark_font = courier 24


data_points :: Dataset Double Double
data_points = 
    [ (0.1, 0.95)
    , (0.2, 0.8) 
    , (0.3, 0.3)
    , (0.4, 0.52) 
    , (0.5, 0.62)
    , (0.6, 0.7)
    , (0.7, 0.5)
    , (0.8, 0.4)
    , (0.9, 0.25) 
    , (1,   0.2)
    ]

