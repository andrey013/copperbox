{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- main... 
--
--------------------------------------------------------------------------------

module Main where

import Hurdle.Pecoff.Datatypes
import Hurdle.Pecoff.DefOutput
import Hurdle.Pecoff.Parser
import Hurdle.Pecoff.TextDump

import System.Console.GetOpt
import System.Environment

data Flag = Usage | Verbose
  deriving (Eq, Show)

header :: String
header = "Usage: pexports file"

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]     (NoArg Usage)    help_text
    , Option ['v'] ["verbose"]  (NoArg Verbose)  verb_text
      
    ]
  where
   help_text = "please use pexports for the time being."   
   verb_text = "(very) verbose mode"
main :: IO ()
main = do
  args <- getArgs
  let (opts, nonopts, errs) = getOpt Permute options args
  main2 opts nonopts errs



main2 :: [Flag] -> [FilePath] -> [String] -> IO ()
main2 opts [fname] [] 
  | Usage `elem`   opts   = putStrLn $ usageInfo header options
  | Verbose `elem` opts   = run printImage fname
  | otherwise             = run printDef   fname

main2 _    _       _      = putStrLn $ "hurdle only handles one file at once."

run :: (Image -> IO ()) -> FilePath -> IO ()
run mf filename = do { img  <- readDLL filename
                     ; mf img
                     ; putStrLn ""  
                     }
  
