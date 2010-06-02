{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


-- See:
-- http://en.wikipedia.org/wiki/Iris_flower_data_set

module Scatter1 where

import IrisParser

import Graphics.PSC.Axis
import Graphics.PSC.Core
import Graphics.PSC.ScatterPlot



import Wumpus.Extra.SafeFonts           -- package: wumpus-core
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
                       ScatterPlot scatter_scale drect 
                                                 (Just grid_cfg)
                                                 (Just axes_cfg)  Nothing $ 
                         [ (sepal_cfg,      map slsw setosa)
                         , (versicolor_cfg, map slsw versicolor)
                         , (virginica_cfg,  map slsw virginica)
                         ]

          writeChartEPS "./out/scatter01.eps" pic
          writeChartSVG "./out/scatter01.svg" pic



drect :: DrawingRectangle
drect = DrawingRectangle 300 300

scatter_scale :: XYProjection Double Double
scatter_scale = (Projection id 4.0 (300/4), Projection id 2.0 (300/2.5))


sepal_cfg       :: DotConfig 
sepal_cfg       = DotConfig red 3

versicolor_cfg  :: DotConfig
versicolor_cfg  = DotConfig green 3

virginica_cfg   :: DotConfig
virginica_cfg   = DotConfig blue 3

slsw :: IrisData -> (Double,Double)
slsw iris = (sepal_length iris, sepal_width iris)


axes_cfg :: AxisLabelConfig Double Double
axes_cfg = AxisLabelConfig
      { label_font      = courier24
      , font_colour     = black
      , x_axis_cfg      = Just (axis_x, ffloat 1)
      , y_axis_cfg      = Just (axis_y, ffloat 1)
      } 


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
     
