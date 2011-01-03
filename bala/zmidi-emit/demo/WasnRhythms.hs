{-# OPTIONS -Wall #-}


module WasnRhythms where

import ZMidi.Emit

import ZMidi.Core                               -- package: zmidi-core

import Control.Monad
import Data.Monoid

main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

demo01 :: IO ()
demo01 = do
    putStrLn "Writing wasn.mid..."
    writeHiMidi "wasn.mid" $ 
      hiMidi `meta`  genericText  "Wasn rhythms"
             `addT`  track 9 perc_track
             
  where
    perc_track  = section 120 fallahy `mappend` section 120 malfuf


makeRhythm :: NoteList () -> NoteList ()
makeRhythm body = replicateM_ 4 body >> rest dwhole

fallahy :: NoteList ()
fallahy = makeRhythm $
    dum (dotted deighth) >> tek dsixteenth >> dum deighth >> tek deighth


malfuf :: NoteList ()
malfuf = makeRhythm $
    dum (dotted deighth) >> tek dsixteenth >> rest deighth >> tek deighth


dum :: MidiDuration -> NoteList ()
dum = note acoustic_bass_drum

tek :: MidiDuration -> NoteList ()
tek = note acoustic_snare

tika :: MidiDuration -> NoteList ()
tika = note open_hi_hat

dotted :: MidiDuration -> MidiDuration
dotted = (1.5 *)

demo02 :: IO ()
demo02 = do
    ans <- readMidi "wasn.mid"
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

