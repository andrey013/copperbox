{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  LGPL (depends on CppHs)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------

module Main where

import CPP
import Precis.Cabal.CabalPackage
import Precis.Cabal.Datatypes
import Precis.HsSrc.Datatypes
import Precis.HsSrc.Utils
import Precis.HtmlReport
import Precis.VersionNumber

import Language.Haskell.Exts ( Module )         -- package: haskell-src-exts

import Text.XHtml ( renderHtml )                -- package: xhtml

import System.Environment
import System.Exit
import System.Console.GetOpt


header :: String
header = "Usage: precis <new_cabal_file> <old_cabal_file>"

help_message :: String
help_message = unlines $  
    [ "Summarize the API differences between two revisions of a" 
    , "Cabal package."
    ]

data Flag = Usage
          | Version
          | HtmlReport String
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]     (NoArg Usage)        help_message
    , Option ['v'] ["version"]  (NoArg Version)      "show version"
    , Option ['o'] ["out"]      (ReqArg HtmlReport "FILE")   
             "output HTML report"
    ]

main :: IO ()
main = do { args <- getArgs
          ; let (opts, nonopts, errs) = getOpt Permute options args
          ; main2 opts nonopts errs
          }
  
main2 :: [Flag] -> [FilePath] -> [String] -> IO ()
main2 opts _           _ 
  | Usage       `elem` opts = precisExit $ usageInfo header options
  | Version     `elem` opts = precisExit $ 
                                "precis version " ++ precis_version_number

main2 opts    [newc,oldc] []   = runCompare (lookupOutFile opts) newc oldc
main2 _       _           errs = 
    precisExitFail 1 (concat errs ++ usageInfo header options)

lookupOutFile :: [Flag] -> Maybe String
lookupOutFile []                 = Nothing
lookupOutFile (HtmlReport out:_) = Just out
lookupOutFile (_:xs)             = lookupOutFile xs

precisExit :: String -> IO ()
precisExit s = putStrLn s >> exitWith ExitSuccess

precisExitFail :: Int -> String -> IO ()
precisExitFail i s = putStrLn s >> exitWith (ExitFailure i)


runCompare :: (Maybe FilePath) -> FilePath -> FilePath -> IO ()
runCompare mb_out new_cabal_file old_cabal_file = do 
    ans1 <- extractPrecis new_cabal_file
    ans2 <- extractPrecis old_cabal_file

    case (ans1,ans2) of
      (Right new_cp, Right old_cp) -> sk new_cp old_cp
      (Left err, _)                -> precisExitFail 2 $ cabalFileErrorMsg err
      (_, Left err)                -> precisExitFail 2 $ cabalFileErrorMsg err
  where
   sk new_cp old_cp = case mb_out of 
                        Nothing -> shortReport new_cp old_cp
                        Just path -> fullReportHtml path new_cp old_cp


fullReportHtml :: FilePath -> CabalPrecis -> CabalPrecis -> IO ()
fullReportHtml out_path new_cp old_cp = 
    do { (my_doc,msg) <- makeFullReport fullParseModule new_cp old_cp
       ; putStrLn $ msg
       ; writeFile out_path (renderHtml my_doc)
       }


shortReport :: CabalPrecis -> CabalPrecis -> IO ()
shortReport new_cp old_cp = 
    do { msg <- makeShortReport fullParseModule new_cp old_cp
       ; putStrLn $ msg
       }

-- | macro-expand and parse
--
fullParseModule :: SourceFile -> IO (Either ModuleParseError Module)
fullParseModule (SourceFile _ file_name) = do
    mx_src <- preprocessFile precisCpphsOptions file_name
    return $ readModule mx_src


