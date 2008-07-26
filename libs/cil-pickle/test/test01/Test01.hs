
module Main where

import CilAbsSyn
import AsdlBase
import CilAsdlPickle

main = do
  ast <- readPickle "test01.c.asdlpkl" readCilFile
  putStr $ show ast


