{-# OPTIONS -Wall #-}


module Demo01 where

import ZMidi.Basic.GeneralMidiInstruments
import ZMidi.Basic.Construction

-- import ZMidi.Basic.VersionNumber

import ZMidi.Core


dummy = c_nat 4

main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

demo01 :: IO ()
demo01 = do
    putStrLn "Writing demo01.mid..."
    writeMidiMCT "demo01.mid" $ [ section1 ]
  where
    section1 = Section 120 [voice1, voice2]
    instr    = instrumentNumber Honky_tonk
    voice1   = SectionVoice instr [ PNote 0.25 default_props $ c_nat 4
                                  , PNote 0.25 default_props $ c_nat 4 ]
    voice2   = SectionVoice instr [ PNote 0.125 default_props $ e_nat 4
                                  , PNote 0.125 default_props $ g_nat 4 ]
   
-- looks like Build needs writer as well?
-- and Primitive probably needs a MidiMsg constructor...
--
temp01 = map (runBuild build_env_zero) $ 
    [ note dquarter (c_nat 4), note dquarter (e_nat 4) ]


default_props :: PrimProps
default_props = PrimProps
      { velocity_on     = 127
      , velocity_off    = 64
      , note_volume     = 127
      }


demo02 :: IO ()
demo02 = do
    ans <- readMidi "demo01.mid"
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

