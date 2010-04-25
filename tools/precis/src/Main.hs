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

import Precis.CabalPackage
import Precis.Datatypes
import Precis.Diff

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
   let diffs = compareModules (cp_exposed_modules new_cp) (cp_exposed_modules old_cp)
   mapM_ print diffs

runExtract :: FilePath -> IO CabalPrecis
runExtract path = do
    ans <- extractPrecis path ["hs", "lhs"]
    case ans of
      Left err -> error $ err
      Right cfg -> return cfg


