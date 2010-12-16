{-# OPTIONS -Wall #-}

-- Dump the contents of a MIDI file

module Main where

import ZMidi.Core

import System.Environment


main :: IO ()
main = do 
  args <- getArgs
  case args of
    [path] -> process path
    _ ->  putStrLn "Usage: MidiPrint <filename>"

process :: FilePath -> IO ()
process filename = do
    ans <- readMidi filename
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

 