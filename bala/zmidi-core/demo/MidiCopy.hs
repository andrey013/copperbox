{-# OPTIONS -Wall #-}

-- Read a MIDI file, output the syntax tree.
-- Are the files the same?


module Main where

import ZMidi.Core

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
      Right a  -> do { mapM_ putStrLn $ printMidiHeader $ mf_header a
                     ; writeMidi (filename ++ ".001") a }
    

 