{-# OPTIONS -Wall #-}


module Demo01 where

import ZMidi.Emit

import ZMidi.Core                               -- package: zmidi-core

import Data.Monoid

import System.Directory

main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

demo01_midi :: FilePath
demo01_midi = "./out/demo01.mid"

demo01 :: IO ()
demo01 = do
    createDirectoryIfMissing True "./out/"
    putStrLn "Writing out/demo01.mid..."
    writeHiMidi demo01_midi $ hiMidi `addTrack` track 0 sections
  where
    sections = monoVoice 120 phrase1 `mappend` monoVoice 120 phrase2
    phrase1  = instrument honky_tonk >> note (c_nat 4) dquarter 
                                     >> note (c_nat 4) dquarter 
    phrase2  = instrument honky_tonk >> 
                 localize (noteOnVelo 64) (    note (e_nat 4) deighth
                                            >> note (g_nat 4) deighth )


    

demo02 :: IO ()
demo02 = do
    ans <- readMidi demo01_midi
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

