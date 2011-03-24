{-# OPTIONS -Wall #-}

--
-- Unfortunately General MIDI drums are a horrible 
-- realization for anything other than \"rock\"...
--

module WasnRhythms where

import ZMidi.Emit

import ZMidi.Core                               -- package: zmidi-core

import Control.Monad
import Data.Monoid
import System.Directory

main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

wasn_midi :: FilePath
wasn_midi = "./out/wasn.mid"


demo01 :: IO ()
demo01 = do
    createDirectoryIfMissing True "./out/"
    putStrLn "Writing wasn.mid..."
    writeHiMidi wasn_midi $ 
      hiMidi `meta`  genericText  "Wasn rhythms"
             `addT`  track 9 perc_track
             
  where
    perc_track  =           monoVoice 100 fallahy 
                  `mappend` monoVoice 100 malfuf
                  `mappend` monoVoice 100 karatum
                  `mappend` monoVoice 80 chiftatelli


makeRhythm :: NoteList () -> NoteList ()
makeRhythm body = replicateM_ 4 body >> rest dwhole

fallahy :: NoteList ()
fallahy = makeRhythm $
    dum (dotted deighth) >> tek dsixteenth >> dum deighth >> tek deighth


malfuf :: NoteList ()
malfuf = makeRhythm $
    dum (dotted deighth) >> tek dsixteenth >> rest deighth >> tek deighth


karatum :: NoteList ()
karatum = makeRhythm $
    tek (dotted deighth) >> tek dsixteenth >> tek deighth >> tek deighth


chiftatelli :: NoteList ()
chiftatelli = makeRhythm $
    dum deighth >> tika dsixteenth >> tika dsixteenth >> 
    tika dsixteenth >> tika dsixteenth >> tek deighth >>
    dum deighth >> dum deighth >> 
    tek dquarter 



dum :: MidiDuration -> NoteList ()
dum = note acoustic_bass_drum

tek :: MidiDuration -> NoteList ()
tek = note acoustic_snare

tika :: MidiDuration -> NoteList ()
tika = note open_triangle

dotted :: MidiDuration -> MidiDuration
dotted = (1.5 *)

demo02 :: IO ()
demo02 = do
    ans <- readMidi wasn_midi
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

