{-# OPTIONS -Wall #-}

module CabalDemo where

import CPP
import Precis.Cabal.CabalPackage
import Precis.Cabal.Datatypes
import Precis.Cabal.PathUtils
import Precis.Diff
import Precis.HsSrc.Datatypes
import Precis.HsSrc.Utils

import Language.Haskell.Exts ( Module, prettyPrint )

import qualified Distribution.ModuleName                as D
import qualified Distribution.PackageDescription.Parse  as D
import qualified Distribution.Verbosity                 as D

import Data.Char
import System.FilePath



runExtract :: FilePath -> IO CabalPrecis
runExtract path = do
    ans <- extractPrecis path
    case ans of
      Left err -> error $ show err
      Right cfg -> return cfg

fullParseModule :: SourceFile -> IO (Either ModuleParseError Module)
fullParseModule (SourceFile _ file_name) = do
    mx_src <- preprocessFile precisCpphsOptions file_name
    return $ readModule mx_src


demo1 :: IO ()
demo1 = runExtract "../../_sample_data/mtl.cabal" >>= print

demo2 :: IO ()
demo2 = do 
  cp <- runExtract "../../_sample_data/regex-posix.cabal"
  print cp

demo2a :: IO ()
demo2a = do
  pd <- D.readPackageDescription  D.verbose "../../_sample_data/regex-posix.cabal"
  print pd


demo3 :: IO ()
demo3 = do 
  ans <- fullParseModule (SourceFile "State.Strict"
                                     "../../_sample_data/Control/Monad/Cont/Class.hs")
                    
  case ans of
    Right modu -> putStrLn $ prettyPrint modu
    Left err   -> error $ show err


demo4 :: IO ()
demo4 = do 
  print $ splitPath "../samples/one.hs" 
  print $ splitPath "..\\samples\\two.hs"
  print $ normalise "../samples/one.hs" 
  print $ normalise "..\\samples\\two.hs"


demo5 :: [Edit4 Char]
demo5 = diff4 match conflict "ABCDEF" "feCba"
  where
    match a b = toLower a == toLower b
    conflict  = (/=)


{-
demo5 :: IO ()
demo5 = do 
   c1 <- runExtract "../../../../source/monadLib-3.6.1/monadLib.cabal"
   c2 <- runExtract "../../../../source/monadLib-3.5.2/monadLib.cabal"
   let diffs = compareModules (exposed_modules c1) (exposed_modules c2)
   mapM_ print diffs
-}

{-

demo6 :: IO ()
demo6 = do
    txt <- preproUseless fname
    let ans = parseModuleWithExts knownExtensions fname txt
    case ans of 
      ParseOk a -> putStr $  prettyPrint a
      ParseFailed loc err -> putStrLn txt >> putStrLn (show (loc,err))
  where
    fname = "../../../../source/monadLib-3.6.1/src/MonadLib.hs"


demo7 :: IO ()
demo7 = print =<< preproTest "#line 3 \"..\" \n\n"

demo8 :: IO ()
demo8 = let ans = parseModuleWithExts knownExtensions "" txt in
    case ans of 
      ParseOk a -> putStr $  prettyPrint a
      ParseFailed loc err -> putStrLn txt >> putStrLn (show (loc,err))
 where
   txt = unlines [ "#line 1 \"f.hs\""
                 , "module Main where"
                 , "main = print 4"
                 , ""
                 ]
-}