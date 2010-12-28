{-# OPTIONS -Wall #-}


module Demo01 where

import ZMidi.Emit

import ZMidi.Core                               -- package: zmidi-core


main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

demo01 :: IO ()
demo01 = do
    putStrLn "Writing demo01.mid..."
    writeZMidiRep "demo01.mid" $ singleTrack $ singleChannel 0 section1
  where
    section1 = singleSection 120 [phrase1, phrase2]
    phrase1  = instrument Honky_tonk >> note dquarter (c_nat 4) 
                                     >> note dquarter (c_nat 4)
    phrase2  = instrument Honky_tonk >> note deighth  (e_nat 4) 
                                     >> note deighth  (g_nat 4)


    

demo02 :: IO ()
demo02 = do
    ans <- readMidi "demo01.mid"
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

