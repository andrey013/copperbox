{-# OPTIONS -Wall #-}

module Spark1 where

import Wumpus.PSC.Bivariate
import Wumpus.PSC.BivariateGraphic
import Wumpus.PSC.Core
import Wumpus.PSC.SparkLine

import Wumpus.Core

import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours 

import System.Directory


main :: IO ()
main =  createDirectoryIfMissing True "./out/"
     >> writeChartEPS "./out/spark1.eps" chart1
     >> writeChartSVG "./out/spark1.svg" chart1

chart1 :: Chart
chart1 = drawGraphicU $ supply spark_bv $ 
            spark_line `cc` spark_rangeband


spark_line :: BivariateGraphic Double Double
spark_line = sparkLine spark_stroke data_points

spark_stroke :: SparkLineF
spark_stroke = simpleLine black 1.0


spark_rangeband :: BivariateGraphic Double Double
spark_rangeband = rangeBand (0.3 ::: 0.8) aquamarine



spark_bv :: Bivariate Double Double
spark_bv = bivariate (0.1 ::: 1.0, id) 
                     (0.0 ::: 1.0, id) 
                     output_rect



output_rect :: RectangleLoc Double
output_rect = (sparklineRectangle spark_font 10, zeroPt)

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

