{-# OPTIONS -Wall #-}

module CabalDemo where

import Precis.Datatypes
import Precis.Diff
import Precis.CabalPackage
import Precis.ModuleExports
import Precis.PathUtils
import Precis.Utils

import Language.Haskell.Exts.Pretty ( prettyPrint )

import Data.Map
import Text.PrettyPrint.Leijen
import System.FilePath



runExtract :: FilePath -> IO CabalPrecis
runExtract path = do
    ans <- extractPrecis path ["hs", "lhs"]
    case ans of
      Left err -> error $ err
      Right cfg -> return cfg
 

demo1 :: IO ()
demo1 = runExtract "../samples/mtl.cabal" >>= print

demo2 :: IO ()
demo2 = runExtract "../../hurdle/hurdle.cabal" >>= print




headModule :: CabalPrecis -> SourceModule
headModule = head . cp_exposed_modules

demo3 :: IO ()
demo3 = do 
  ans <- extractExports (sourceModule "State.Strict" 
                                      "../samples/Control/Monad/State/Strict.hs")
  case ans of
    Right (exps,fm) -> putDoc80 (pretty exps) >> mapM_ (putStrLn . prettyPrint) (elems fm)
    Left err       -> error err


demo4 :: IO ()
demo4 = do 
  print $ splitPath "../samples/one.hs" 
  print $ splitPath "..\\samples\\two.hs"
  print $ normalise "../samples/one.hs" 
  print $ normalise "..\\samples\\two.hs"
  print $ removePrefix "../samples/one.hs" "..\\samples\\two.hs"


demo5 :: IO ()
demo5 = do 
   c1 <- runExtract "../../../../source/monadLib-3.6.1/monadLib.cabal"
   c2 <- runExtract "../../../../source/monadLib-3.5.2/monadLib.cabal"
   let diffs = compareModules (cp_exposed_modules c1) (cp_exposed_modules c2)
   mapM_ print diffs
