{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


-- See:
-- http://en.wikipedia.org/wiki/Iris_flower_data_set

module Scatter where

import IrisParser
import Graphics.PSC.ScatterPlot

import Wumpus.Extra.SVGColours



import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    sequence_ [ demo01 ]

demo01 :: IO ()
demo01 = do 
    ans <- readIrisData
    case ans of
      Nothing -> putStrLn "no go"
      Just (setosa, _versicolor, _virginica) -> do 
          let pic = drawScatterPlot attrs1 (sepalProps, map slsw setosa)
          writeScatterPlotEPS "./out/scatter01.eps" pic
          writeScatterPlotSVG "./out/scatter01.svg" pic

sepalProps :: ScatterPlotProps 
sepalProps = ScatterPlotProps 3 red

attrs1 :: ScatterPlotConfig Double Double  
attrs1 = ScatterPlotConfig
             { plot_width          = 200
             , plot_height         = 200
             , x_range             = (4.0, 6.0, id)
             , y_range             = (2.0, 5.0, id)
             }

slsw :: IrisData -> (Double,Double)
slsw iris = (sepal_length iris, sepal_width iris)

