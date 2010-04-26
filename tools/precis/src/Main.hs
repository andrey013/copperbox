{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
--
--------------------------------------------------------------------------------


module Main where

import Precis.CPP
import Precis.CabalPackage
import Precis.Datatypes
import Precis.Diff
import Precis.ModuleExports
import Precis.Utils

-- import Text.PrettyPrint.Leijen                    -- package: wl-pprint

-- import System.IO ( stdout )
import System.Environment
import System.Console.GetOpt


header :: String
header = "Usage: precis <new_cabal_file> <old_cabal_file>"

data Flag = Usage
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]     (NoArg Usage)        "no help yet"
    ]

main :: IO ()
main = do { args <- getArgs
          ; let (opts, nonopts, errs) = getOpt Permute options args
          ; main2 opts nonopts errs
          }
  
main2 :: [Flag] -> [FilePath] -> [String] -> IO ()
main2 opts [new_cabal,old_cabal] [] 
  | Usage       `elem` opts = putStrLn $ usageInfo header options
  | otherwise               = runCompare new_cabal old_cabal
  
main2  _ _ errors = putStrLn (concat errors ++ usageInfo header options)



runCompare :: FilePath -> FilePath -> IO ()
runCompare new_cabal_file old_cabal_file = do 
   new_cp <- runExtract new_cabal_file
   old_cp <- runExtract old_cabal_file
   let docTL = summarizeTopLevelChanges new_cp old_cp
   putDoc80 docTL
   -- module diffs
   let (expos,_) = moduleDiffLists new_cp old_cp
   mapM_ compareModuleDiff expos

compareModuleDiff :: Diff SourceFile -> IO ()
compareModuleDiff (InL a)      = either putStrLn stat1 =<< fullParseModule a
compareModuleDiff (InR a)      = putStrLn $ (module_name a) ++ " only in old"
compareModuleDiff (InBoth n o) = do 
  ans1 <- fullParseModule n
  ans2 <- fullParseModule o
  case (ans1,ans2) of 
    (Right new,Right old) -> 
        let result = moduleDifferences (module_name n) new old
        in putDoc80 result
    (Left err, _) -> putStrLn err
    (_, Left err) -> putStrLn err


-- | macro-expand and parse
--
fullParseModule :: SourceFile -> IO (Either ModuleParseErr ModulePrecis)
fullParseModule (UnresolvedFile name) = return (Left $ "FileErr " ++ name)
fullParseModule (SourceFile modu_name file_name) = do
  mx_src <- preprocessFile precisCpphsOptions file_name
  return $ readModule modu_name mx_src



stat1 :: ModulePrecis -> IO ()
stat1 (ModulePrecis ep _) = putStrLn $ (mep_base_module ep) ++ " only in new"


runExtract :: FilePath -> IO CabalPrecis
runExtract path = do
    ans <- extractPrecis path ["hs", "lhs"]
    case ans of
      Left  err -> error $ err
      Right cfg -> return cfg

