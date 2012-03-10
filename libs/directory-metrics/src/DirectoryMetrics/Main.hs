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

import DirectoryMetrics.ParserCombinators
import DirectoryMetrics.WindowsParser


import System.IO
import System.Environment
import System.Console.GetOpt


data Flag = Usage
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
    parseFromFile path toplevels >>= either print print
