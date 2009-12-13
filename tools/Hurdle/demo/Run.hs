
-- Hurdle - haskell utility regarding dll exports

module Run where

import Hurdle.Datatypes
import Hurdle.ParseMonad
import Hurdle.Parser
import Hurdle.TextDump


main :: IO ()
main = demo

demo = do 
  img  <- readDLL "dll/SDL.dll"
  printImage img
  putStrLn ""  

