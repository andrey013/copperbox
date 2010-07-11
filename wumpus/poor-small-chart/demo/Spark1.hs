{-# OPTIONS -Wall #-}

module Spark1 where

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils
import Graphics.PSC.SparkLine

import Wumpus.Core
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours 




import System.Directory

main :: IO ()
main =  createDirectoryIfMissing True "./out/"
     >> writeChartEPS "./out/spark1.eps" chart1
     >> writeChartSVG "./out/spark1.svg" chart1

chart1 :: Chart
chart1 = renderSparkLine (SparkLine spark_ctx spark_stroke
                                    spark_range) 
                         data_points

spark_stroke :: SparkLineF 
spark_stroke = simpleLine black 0.5

spark_range :: RangeBandF Double Double
spark_range = rangeBand (0.3 ::: 0.8) aquamarine

spark_ctx :: DrawingContext Double Double
spark_ctx = drawingContext (0.1 ::: 1.0) id (0.0 ::: 1.0) id rect
  where
    rect = sparklineRectangle spark_font 10

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

