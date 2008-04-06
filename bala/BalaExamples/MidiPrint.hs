

-- ghci> :set -i..
-- ghci> :set args ../Variant/bulgarian6ly.midi

-- to get a parse failure
-- ghci> :set args ../Variant/tintal.mid


-- or shell> ghc --make MidiPrint.hs -i..

module Main where

import Bala.Format.Midi.Midi

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
  (ans,w) <- readMidi filename
  putStr w
  case ans of
    Left err -> putStrLn $ "Parse error " ++ err
    Right mf@(MidiFile _ ts) -> do 
      putDoc (pretty mf)

process' :: FilePath -> IO ()
process' filename = do
  (ans,w) <- readWords filename
  putStr w
  case ans of
    Left err -> putStrLn $ "Parse error " ++ err
    Right a -> putStrLn $ "Parse end, " ++ show (length a) ++ " words read"
 