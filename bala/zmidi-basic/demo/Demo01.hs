{-# OPTIONS -Wall #-}


module Demo01 where

import ZMidi.Basic.GeneralMidiInstruments
import ZMidi.Basic.WriteMidi.Top
import ZMidi.Basic.VersionNumber

import ZMidi.Core



demo01 = writeMidiMCT "demo01.mid" $ 
    [ [ PNote 0.25 default_props 60
      , PNote 0.25 default_props 62 
      ]
    , [ PNote 0.125 default_props 80
      , PNote 0.125 default_props 83
      ]
    ]


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

