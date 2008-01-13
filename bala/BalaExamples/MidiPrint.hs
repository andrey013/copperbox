

-- ghci> :set -i..
-- ghci> :set args ../Variant/bulgarian6ly.midi

-- or shell> ghc --make MidiPrint.hs -i..

module Main where

import Bala.Format.Midi.MidiFile 

import System.Environment
import Text.PrettyPrint.Leijen


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
    Left err -> putStrLn $ "Parse error " ++ err
    Right mf@(MidiFile _ ts) -> do 
      putDoc (pretty mf)

 