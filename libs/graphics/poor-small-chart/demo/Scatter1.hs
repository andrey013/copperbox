{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


module Scatter where

import IrisParser
import Graphics.PSC.ScatterPlot
import Graphics.PSC.Utils

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
          let pic = drawScatterPlot black (map sepalData setosa)
          writeScatterPlotEPS "./out/scatter01.eps" pic
          writeScatterPlotSVG "./out/scatter01.svg" pic
           

sepalData :: IrisData -> (Double,Double)
sepalData iris = (slRescale $ sepal_length iris, swRescale $ sepal_width iris)

slRescale :: Double -> Double
slRescale = rescale 4 6 0 100

swRescale :: Double -> Double
swRescale = rescale 2 5 0 100

