

-- ghci> :set -i..
-- ghci> :set args bulgarian6.midi


-- shell> ghc --make MidiPrint.hs -i..
--
-- shell> runhaskell.exe -i.. MidiPrint.hs bulgarian6.midi



module Main where

import Bala.Format.Midi
import qualified Bala.Format.Midi.ReadFileAlt as Alt

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
  ans <- Alt.readMidi filename
  putDoc (pretty ans)


 