{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


-- See:
-- http://en.wikipedia.org/wiki/Iris_flower_data_set

module Scatter where

import IrisParser
import Graphics.PSC.ScatterPlot

import Wumpus.Extra.SafeFonts           -- package: wumpus-core
import Wumpus.Extra.SVGColours

import System.Directory

import Graphics.PSC.Axis -- TEMP


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    sequence_ [ demo01 ]

demo01 :: IO ()
demo01 = do 
    ans <- readIrisData
    case ans of
      Nothing -> putStrLn "no go"
      Just (setosa, versicolor, virginica) -> do 
          let pic = drawMulti attrs1 x_axis_label $ 
                     [ (sepalProps, map slsw setosa)
                     , (versicolorProps, map slsw versicolor)
                     , (virginicaProps, map slsw virginica)
                     ]

          writeScatterPlotEPS "./out/scatter01.eps" pic
          writeScatterPlotSVG "./out/scatter01.svg" pic

sepalProps :: ScatterPlotProps 
sepalProps = ScatterPlotProps 3 red

versicolorProps :: ScatterPlotProps 
versicolorProps = ScatterPlotProps 3 green

virginicaProps :: ScatterPlotProps 
virginicaProps = ScatterPlotProps 3 blue


attrs1 :: ScatterPlotConfig Double Double  
attrs1 = ScatterPlotConfig
             { plot_width          = 200
             , plot_height         = 200
             , x_range             = (4.0, 8.0, id)
             , y_range             = (2.0, 6.0, id)
             }

slsw :: IrisData -> (Double,Double)
slsw iris = (sepal_length iris, sepal_width iris)

-- x-axis 4.5, 5.5, 6.5, 7.5
-- y-axis 2.0, 2.5, 3.0, 3.5, 4.0

x_axis_label :: AxisLabel Double
x_axis_label = AxisLabel
      { label_font      = helvetica10
      , start_value     = 4.5
      , step_count      = 4
      , step_fun        = (+1.0)
      , render_fun      = ffloat 1
      }