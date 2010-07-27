{-# OPTIONS -Wall #-}

-- ghci> :set -i../../ZMidi
-- ghci> :set args bulgarian6.mid


-- shell> ghc --make MidiPrint.hs -i../../ZMidi
--
-- shell> runhaskell.exe -i../../ZMidi MidiPrint.hs bulgarian6.mid



module Test01 where


import ZMidi.Core.Datatypes
import ZMidi.Core.ReadFile


import Control.Exception
import Prelude hiding (catch)

import System.Environment
import System.Exit


test01 = process "midifiles/bilawal.mid"
test02 = process "midifiles/bilawal_khyal.mid"
test03 = process "midifiles/mfmorty2.mid"

process :: FilePath -> IO ()
process filename = do
    ans <- catch (readMidi filename) exitHandle
    putStrLn $ show ans                         -- not very good...
  where
    exitHandle :: IOException -> IO a 
    exitHandle e = putStrLn (show e) >> exitFailure

 