-- :set -i./External


-- :set args -w 80 --dir=./test/output --name=Cil ../../resources/cil.asdl ../../resources/cil-views.asdl
-- OR:
-- :set args --dir=./test/output ./test/mod_example.asdl
-- OR:
-- :set args --dir=./test/output ./test/sexp1.asdl
-- OR:
-- :set args --dir=./test/output/regen ./asdl_defs/asdl.asdl ./asdl_defs/ocaml.asdl ./asdl_defs/uuag.asdl 
-- :set args --dir=./test/output --name=UUAG ./asdl_defs/uuag.asdl

module Main where


import Base.Lib
import Compiler.Lib

import Control.Monad
import System.Environment
import System.Console.GetOpt
 
    
main :: IO ()
main = do
  args <- getArgs
  let (opts, nonopts, errs) = getOpt Permute options args
  main2 opts nonopts errs

         
main2 :: [Flag] -> [FilePath] -> [String] -> IO ()
main2 opts files [] 
  | Flag_usage `elem` opts  = putStrLn $ usageInfo header options
  | otherwise               = runCompile opts files 

               
                                             
main2  _ _ errors = putStrLn (concat errors ++ usageInfo header options)         







