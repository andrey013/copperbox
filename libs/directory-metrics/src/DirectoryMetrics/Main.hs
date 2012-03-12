{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.Main
-- Copyright   :  (c) Stephen Peter Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Parser for output from Windows @dir@ command.
--
-----------------------------------------------------------------------------


module DirectoryMetrics.Main where

import DirectoryMetrics.HierSyntax
import DirectoryMetrics.ParserCombinators
import DirectoryMetrics.SizeMetrics
import DirectoryMetrics.StructureMetrics
import DirectoryMetrics.TreeView
import DirectoryMetrics.WindowsParser


-- import System.IO
import System.Environment
import System.Console.GetOpt


data Flag = Top_Is_Tree |  Usage
  deriving (Eq, Show)

header :: String 
header = unlines $ 
    [ "Usage: dirmetz.exe <directory_listing_file> "
    , ""
    , "Works on listings collected by |> dir /s <| and not directly"
    , "on the file system."
    ]


options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]     (NoArg Usage)        "show this help"
    , Option ['t'] ["tree"]     (NoArg Top_Is_Tree)  "treat root as tree not forest"
    ]

main :: IO ()
main = do{ args <- getArgs
         ; let (opts, nonopts, errs) = getOpt Permute options args
         ; main2 opts nonopts errs
         }

main2 :: [Flag] -> [FilePath] -> [String] -> IO ()
main2 opts [prime] [] 
  | Usage       `elem` opts = putStrLn $ usageInfo header options
  | otherwise               = action1 prime
  
main2  _ _ errors = putStrLn (concat errors ++ usageInfo header options)


action1 :: FilePath -> IO ()
action1 path = 
    parseFromFile path toplevels >>= either print sk 
  where
    sk = drawDirectories . streamFlat


bothMetrics :: Directory -> (SizeMetrics,StructMetrics)
bothMetrics d = (sizeMetrics d, structMetrics d)

bothMetricsF :: [Directory] -> [(SizeMetrics,StructMetrics)]
bothMetricsF = map bothMetrics

