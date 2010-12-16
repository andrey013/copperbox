{-# OPTIONS -Wall #-}


module Demo01 where

import ZMidi.Basic.GeneralMidiInstruments
import ZMidi.Basic.WriteMidi.Top
import ZMidi.Basic.VersionNumber

import ZMidi.Core



demo01 = writeMidi "demo01.mid" $ outputMCST $ wrap
    [ [ PNote 532 default_props 60
      , PNote 532 default_props 62 
      ]
    , [ PNote 266 default_props 80
      , PNote 266 default_props 83
      ]
    ]

wrap a = [a]

default_props = PrimProps
      { velocity_on     = 127
      , velocity_off    = 127
      , note_volume     = 127
      }


demo02 :: IO ()
demo02 = do
    ans <- readMidi "demo01.mid"
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

