{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


-- See:
-- http://en.wikipedia.org/wiki/Iris_flower_data_set

module ScatterTWO where

import IrisParser

import Graphics.PSC.Core
import Graphics.PSC.ScatterPlotTWO

import Graphics.PSC.AxisTWO


-- import Wumpus.Extra.SafeFonts           -- package: wumpus-core
import Wumpus.Extra.SVGColours

import System.Directory



main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> demo01

demo01 :: IO ()
demo01 = do 
    ans <- readIrisData
    case ans of
      Nothing -> putStrLn "no go"
      Just (setosa, versicolor, virginica) -> do 
          let pic =  renderScatterPlot $ 
                       ScatterPlot scatter_scale drect (Just grid_cfg) Nothing $ 
                         [ (sepal_cfg,      map slsw setosa)
                         , (versicolor_cfg, map slsw versicolor)
                         , (virginica_cfg,  map slsw virginica)
                         ]

          writeChartEPS "./out/scatter02.eps" pic
          writeChartSVG "./out/scatter02.svg" pic

drect :: DrawingRectangle
drect = DrawingRectangle 300 300

scatter_scale :: XYProjection Double Double
scatter_scale = (Projection id 4.0 (300/4), Projection id 2.0 (300/2.5))


sepal_cfg       :: LayerConfiguration 
sepal_cfg       = LayerConfiguration red 3

versicolor_cfg  :: LayerConfiguration
versicolor_cfg  = LayerConfiguration green 3

virginica_cfg   :: LayerConfiguration
virginica_cfg   = LayerConfiguration blue 3

slsw :: IrisData -> (Double,Double)
slsw iris = (sepal_length iris, sepal_width iris)


grid_cfg :: GridConfig Double Double
grid_cfg = GridConfig
      { grid_line       = LineConfiguration blue 0.5 Nothing
      , grid_x_axis     = Just axis_x
      , grid_y_axis     = Just axis_y
      } 

axis_x :: AxisLabelAlg Double
axis_x = AxisLabelAlg
           { start_value     = 4.5
           , step_fun        = (+1.0)
           }

axis_y :: AxisLabelAlg Double
axis_y = AxisLabelAlg
           { start_value     = 2.0
           , step_fun        = (+0.5)
           }
     
