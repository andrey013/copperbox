{-# OPTIONS -Wall #-}


module Scale where

import ZMidi.Emit

import ZMidi.Core                               -- package: zmidi-core

import Data.Monoid
import System.Directory

main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

outfile_midi :: FilePath
outfile_midi = "./out/scale.mid"

demo01 :: IO ()
demo01 = do
    createDirectoryIfMissing True "./out/"
    putStrLn $ "Writing " ++ outfile_midi
    writeHiMidi outfile_midi $ 
      hiMidi `meta`  genericText  "C major scale"
             `addT`  track 0 scale_track
             

scale_track :: Voice
scale_track  = monoVoice 120 c_major_up `mappend` monoVoice 120 c_major_down



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
    ans <- readMidi outfile_midi
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

