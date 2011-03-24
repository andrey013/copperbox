{-# OPTIONS -Wall #-}


module Perc where

import ZMidi.Emit

import ZMidi.Core                               -- package: zmidi-core

import System.Directory

-- import Data.Monoid

main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute


outfile_midi :: FilePath
outfile_midi = "./out/perc01.mid"

demo01 :: IO ()
demo01 = do
    createDirectoryIfMissing True "./out/"
    putStrLn "Writing perc01.mid..."
    writeHiMidi outfile_midi $ 
      hiMidi `meta`  lyrics           "please clap..." 
             `meta`  copyrightNotice  "Public domain"
             `addT`  track 9 perc_track
             
  where
    perc_track  = polyVoice 120 [claves1, cabasa1]
    
    claves1     = note claves dquarter >> note claves dquarter
    cabasa1     = note cabasa deighth  >> note cabasa deighth


    

demo02 :: IO ()
demo02 = do
    ans <- readMidi outfile_midi
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

