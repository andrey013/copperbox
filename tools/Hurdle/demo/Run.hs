
-- Hurdle - haskell utility regarding dll exports

module Run where

import Hurdle.Datatypes
import Hurdle.Parser
import Hurdle.TextDump


main :: IO ()
main = demo

demo = do 
  img  <- readDLL "dll/openvg32.dll"
  printImage img
  putStrLn ""  

