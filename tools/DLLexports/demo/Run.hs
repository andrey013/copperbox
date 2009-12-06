
module Run where

import DLLexports.Datatypes
import DLLexports.ParseMonad
import DLLexports.ReadFile
import DLLexports.TextDump


import Numeric

main :: IO ()
main = demo

demo = do 
  img  <- readDLL "dll/openvg32.dll"
  printImage img
  putStrLn ""  

