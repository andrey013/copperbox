
-- Hurdle - haskell utility regarding dll exports

module Run where

import Hurdle.Datatypes
import Hurdle.Parser
import Hurdle.TextDump


main :: IO ()
main = demo2


demo1 = do 
  img  <- readDLL "dll/openvg32.dll"
  printImage img
  putStrLn ""  


demo2 = do 
  img  <- readDLL "dll/libportaudio.dll"
  printImage img
  putStrLn ""  

