{-# OPTIONS -Wall #-}


-- shell> ghc --make PrintMIDI.hs 
--
-- shell> runhaskell.exe PrintMIDI.hs bulgarian6.mid



module Main where

-- TEMP
import Text.PrettyPrint.JoinPrint

import MidiRead
import MidiText


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

 