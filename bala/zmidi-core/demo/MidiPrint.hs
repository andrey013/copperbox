{-# OPTIONS -Wall #-}

-- ghci> :set -i../../ZMidi
-- ghci> :set args bulgarian6.mid


-- shell> ghc --make MidiPrint.hs -i../../ZMidi
--
-- shell> runhaskell.exe -i../../ZMidi MidiPrint.hs bulgarian6.mid



module Main where

import ZMidi

import Control.Exception
import Prelude hiding (catch)

import System.Environment
import System.Exit


main :: IO ()
main = do 
  args <- getArgs
  case args of
    [path] -> process path
    _ ->  putStrLn "Usage: MidiPrint <filename>"

process :: FilePath -> IO ()
process filename = do
    ans <- catch (readMidi filename) exitHandle
    printMidi ans
  where
    exitHandle :: IOException -> IO a 
    exitHandle e = putStrLn (show e) >> exitFailure

 