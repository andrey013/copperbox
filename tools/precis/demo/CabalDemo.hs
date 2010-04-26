{-# OPTIONS -Wall #-}

module CabalDemo where

import Precis.CPP
import Precis.CabalPackage
import Precis.Datatypes
import Precis.Diff
import Precis.HsSrcUtils
import Precis.ModuleExports
import Precis.PathUtils
import Precis.Utils

import Language.Haskell.Exts hiding ( name )

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
demo1 = runExtract "../../_sample_data/mtl.cabal" >>= putDoc . pretty

demo2 :: IO ()
demo2 = do 
  cp <- runExtract "../../_sample_data/mtl.cabal"
  putDoc $ pretty cp
  mods <- exposedModules cp
  mapM_ (either print (putDoc . pretty)) $ elems mods




headModule :: CabalPrecis -> SourceFile
headModule = head . cp_exposed_modules

demo3 :: IO ()
demo3 = do 
  ans <- readModule "../../_sample_data/Control/Monad/Cont/Class.hs" "State.Strict" 
                    
  case ans of
    Right (ModulePrecis exps fm) -> putDoc80 (pretty exps) >> 
                                    mapM_ (putStrLn) (elems fm)
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

demo6 :: IO ()
demo6 = do
    txt <- preproUseless fname
    let ans = parseModuleWithExts knownExtensions fname txt
    case ans of 
      ParseOk a -> putStr $  prettyPrint a
      ParseFailed loc err -> putStrLn txt >> putStrLn (show (loc,err))
  where
    fname = "../../../../source/monadLib-3.6.1/src/MonadLib.hs"