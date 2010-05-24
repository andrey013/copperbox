{-# OPTIONS -Wall #-}

module Sparky1 where

import Graphics.PSC.SparkLine
import Wumpus.Extra.SVGColours 

import System.Directory

main :: IO ()
main = createDirectoryIfMissing True "./out/"   >> 
       sequence_ [ demo1 ]

demo1 :: IO ()
demo1 = writeSparkLineSVG "./out/spark1.svg" pic1 >>
        writeSparkLineEPS "./out/spark1.eps" pic1

attrs1 :: SparkAttr   
attrs1 = SparkAttr 
             { point_size          = 14
             , line_colour         = black
             , horizontal_factor   = 4.0
             , hrange              = Just (0.3, 0.8, aquamarine)
             }


pic1 :: SparkLine
pic1 = drawSparkLine attrs1 spark1


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