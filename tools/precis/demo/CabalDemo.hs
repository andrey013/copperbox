{-# OPTIONS -Wall #-}

module CabalDemo where

import Precis.Datatypes
import Precis.CabalPackage
import Precis.ModuleExports
import Precis.PathUtils
import Precis.Utils

import Text.PrettyPrint.Leijen
import System.FilePath

runExtract :: FilePath -> IO ()
runExtract path = do
    ans <- extractPrecis path ["hs", "lhs"]
    case ans of
      Left err -> error $ err
      Right cfg -> print $ pretty cfg
 

demo1 :: IO ()
demo1 = runExtract "../samples/mtl.cabal"

demo2 :: IO ()
demo2 = runExtract "../../hurdle/hurdle.cabal"



headModule :: CabalPrecis -> SourceModule
headModule = head . cp_exposed_modules

demo3 :: IO ()
demo3 = do 
  ans <- extractExports (sourceModule "State.Strict" 
                                      "../samples/Control/Monad/State/Strict.hs")
  case ans of
    Right exps -> putDoc80 $ pretty exps
    Left err   -> error err


demo4 :: IO ()
demo4 = do 
  print $ splitPath "../samples/one.hs" 
  print $ splitPath "..\\samples\\two.hs"
  print $ normalise "../samples/one.hs" 
  print $ normalise "..\\samples\\two.hs"
  print $ removePrefix "../samples/one.hs" "..\\samples\\two.hs"