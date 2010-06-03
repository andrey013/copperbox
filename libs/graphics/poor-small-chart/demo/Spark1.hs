{-# OPTIONS -Wall #-}

module Spark1 where

import Graphics.PSC.Core
import Graphics.PSC.SparkLine

import Wumpus.Core ( textWidth, textHeight )
import Wumpus.Extra.SVGColours 



import System.Directory

main :: IO ()
main =  createDirectoryIfMissing True "./out/"
     >> writeChartEPS "./out/spark1.eps" chart1
     >> writeChartSVG "./out/spark1.svg" chart1

chart1 :: Chart
chart1 = renderSparkLine (SparkLine spark_size 
                                    spark_scale 
                                    spark_stroke 
                                    data_points)

spark_stroke :: LineConfig
spark_stroke = LineConfig black 0.5 Nothing

spark_size :: SparkLineConfig Double
spark_size = SparkLineConfig 24 10 (Just (aquamarine, 0.3 ::: 0.8))

spark_scale :: XYProjection Double Double
spark_scale = ( Projection id (0.1) (w / 0.9)
              , Projection id 0      h  )

  where
    (w,h) = (textWidth 24 10, textHeight 24)

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

