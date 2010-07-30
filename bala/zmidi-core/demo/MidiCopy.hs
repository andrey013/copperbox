{-# OPTIONS -Wall #-}

-- Read a MIDI file, output the syntax tree.
-- Are the files the same?


module Main where

import ZMidi.Core.ReadFile
import ZMidi.Core.WriteFile


import System.Environment


main :: IO ()
main = do 
  args <- getArgs
  case args of
    [path] -> process path
    _      -> mapM_ putStrLn $ 
              [ "Usage: MidiCopy <filename>"
              , "--"
              , "Read the file, building a syntax tree, print the syntax tree."
              , "Tests that read and write are isomorphic." 
              ]


process :: FilePath -> IO ()
process filename = do
    ans <- readMidi filename
    case ans of
      Left err -> print err
      Right a  -> writeMidi (filename ++ ".001") a
    putStrLn $ take 1000 $ show ans  -- not very good, need a pretty printer...

 