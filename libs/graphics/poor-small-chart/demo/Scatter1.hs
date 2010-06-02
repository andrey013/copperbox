{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


-- See:
-- http://en.wikipedia.org/wiki/Iris_flower_data_set

module Scatter1 where

import IrisParser

import Graphics.PSC.Axis
import Graphics.PSC.Core
import Graphics.PSC.ScatterPlot



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


sepal_cfg       :: LayerConfig 
sepal_cfg       = LayerConfig red 3

versicolor_cfg  :: LayerConfig
versicolor_cfg  = LayerConfig green 3

virginica_cfg   :: LayerConfig
virginica_cfg   = LayerConfig blue 3

slsw :: IrisData -> (Double,Double)
slsw iris = (sepal_length iris, sepal_width iris)


grid_cfg :: GridConfig Double Double
grid_cfg = GridConfig
      { grid_line       = LineConfig blue 0.5 Nothing
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
     
