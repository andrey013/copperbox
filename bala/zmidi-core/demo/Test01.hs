{-# OPTIONS -Wall #-}

-- ghci> :set -i../../ZMidi
-- ghci> :set args bulgarian6.mid


-- shell> ghc --make MidiPrint.hs -i../../ZMidi
--
-- shell> runhaskell.exe -i../../ZMidi MidiPrint.hs bulgarian6.mid



module Test01 where

import ZMidi.Core.Datatypes
import ZMidi.Core.ReadFile
import ZMidi.Core.WriteFile


import System.Environment
import System.Exit

dummy01 = toVarlen 230


test01, test02, test03 :: IO ()
test01 = process "midifiles/bilawal.mid"
test02 = process "midifiles/bilawal_khyal.mid"
test03 = process "midifiles/mfmorty2.mid"
test04 = process "midifiles/bulgarian6.mid"


process :: FilePath -> IO ()
process filename = do
    ans <- readMidi filename
    case ans of
      Left err -> print err
      Right a  -> writeMidi (filename ++ ".001") a
    putStrLn $ take 1000 $ show ans  -- not very good, need a pretty printer...

 