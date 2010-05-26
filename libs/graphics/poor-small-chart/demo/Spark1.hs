{-# OPTIONS -Wall #-}

module Sparky1 where

import Graphics.PSC.SparkLine
import Graphics.PSC.Utils 

import Wumpus.Extra.SVGColours 

import System.Directory

main :: IO ()
main = createDirectoryIfMissing True "./out/"   >> 
       sequence_ [ demo1 ]

demo1 :: IO ()
demo1 = writeSparkLineSVG "./out/spark1.svg" pic1 >>
        writeSparkLineEPS "./out/spark1.eps" pic1

attrs1 :: SparkLineConfig Double Double  
attrs1 = SparkLineConfig
             { point_size          = 24
             , word_length         = 10
             , y_band              = Just (aquamarine, 0.3, 0.8)
             , x_rescale           = rescale 0.1 1 0 100
             , y_rescale           = rescale 0   1 0 100
             }



pic1 :: SparkLine
pic1 = drawSparkLine attrs1 ((SparkLineProps 0.5 black),spark1)



spark1 :: [(Double,Double)]
spark1 = [ (0.1, 0.95)
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