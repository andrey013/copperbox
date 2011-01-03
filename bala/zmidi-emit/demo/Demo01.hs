{-# OPTIONS -Wall #-}


module Demo01 where

import ZMidi.Emit

import ZMidi.Core                               -- package: zmidi-core

import Data.Monoid

main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

demo01 :: IO ()
demo01 = do
    putStrLn "Writing demo01.mid..."
    writeHiMidi "demo01.mid" $ hiMidi `addTrack` track 0 sections
  where
    sections = section 120 phrase1 `mappend` section 120 phrase2
    phrase1  = instrument honky_tonk >> note (c_nat 4) dquarter 
                                     >> note (c_nat 4) dquarter 
    phrase2  = instrument honky_tonk >> 
                 localize (noteOnVelo 64) (    note (e_nat 4) deighth
                                            >> note (g_nat 4) deighth )


    

demo02 :: IO ()
demo02 = do
    ans <- readMidi "demo01.mid"
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

