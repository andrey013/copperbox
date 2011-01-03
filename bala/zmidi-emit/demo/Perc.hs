{-# OPTIONS -Wall #-}


module Perc where

import ZMidi.Emit

import ZMidi.Core                               -- package: zmidi-core

-- import Data.Monoid

main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

demo01 :: IO ()
demo01 = do
    putStrLn "Writing perc01.mid..."
    writeHiMidi "perc01.mid" $ 
      hiMidi `meta`  lyrics           "please clap..." 
             `meta`  copyrightNotice  "Public domain"
             `addT`  track 9 perc_track
             
  where
    perc_track  = overlays 120 [claves1, cabasa1]
    
    claves1     = note claves dquarter >> note claves dquarter
    cabasa1     = note cabasa deighth  >> note cabasa deighth


    

demo02 :: IO ()
demo02 = do
    ans <- readMidi "perc01.mid"
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

