

-- ghci> :set -i..
-- ghci> :set args bulgarian6.midi


-- shell> ghc --make MidiPrint.hs -i..
--
-- shell> runhaskell.exe -i.. MidiPrint.hs bulgarian6.midi



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
  ans <- readMidi filename
  putDoc (pretty ans)


 