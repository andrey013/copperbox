

-- ghci> :set -i../ZMidi
-- ghci> :set args out/bulgarian6.midi


-- shell> ghc --make MidiPrint.hs -i../ZMidi
--
-- shell> runhaskell.exe -i../ZMidi MidiPrint.hs out/bulgarian6.midi



module Main where

import ZMidi
import qualified ZMidi.ReadFileAlt as Alt

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


 