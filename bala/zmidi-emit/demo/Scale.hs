{-# OPTIONS -Wall #-}


module Scale where

import ZMidi.Emit

import ZMidi.Core                               -- package: zmidi-core

import Data.Monoid

main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

demo01 :: IO ()
demo01 = do
    putStrLn "Writing scale.mid..."
    writeHiMidi "scale.mid" $ 
      hiMidi `meta`  genericText  "C major scale"
             `addT`  track 0 scale_track
             

scale_track :: ChannelStream
scale_track  = section 120 c_major_up `mappend` section 120 c_major_down



c_major_up :: NoteList ()
c_major_up = 
    mapM_ qn [ c_nat 4, d_nat 4, e_nat 4, f_nat 4, g_nat 4, a_nat 4, b_nat 4
             , c_nat 5 ]

-- c5 already played ...
c_major_down :: NoteList ()
c_major_down = 
    mapM_ qn [ b_nat 4, a_nat 4, g_nat 4, f_nat 4, e_nat 4, d_nat 4, c_nat 4 ]


qn :: MidiPitch -> NoteList ()
qn pch = note pch dquarter


demo02 :: IO ()
demo02 = do
    ans <- readMidi "scale.mid"
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

