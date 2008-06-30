
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Midi.PerformMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 'Perform' MIDI 
--
--------------------------------------------------------------------------------


module Bala.Format.Midi.PerformMidi (
  MidiEnv(..), default_env, output
) where

import Bala.Format.Midi.Datatypes
import Bala.Format.Midi.WriteFile
import Bala.Format.Midi.GeneralMidiInstruments
import qualified Bala.Perform.Perform as P  
import Bala.Base.Base


import Data.List (sortBy)

output evts env = P.output evts env perform_midi



data MidiEnv = MidiEnv {
  output_file :: FilePath,
  tempo       :: Int,
  gm_inst     :: GMInst
  }

default_env = MidiEnv {
  output_file = "temp.mid",
  tempo       = 120,
  gm_inst     = Acoustic_grand_piano
  }
 

perform_midi = P.Perform {
  P.render = renderMidi,
  P.perform = performMidi
  }
  
  
-- The code below is hacked from something else and needs working through...  


-- Rather than render to a pair of Note on / Note off 
-- in one go, render to a `SimpleEvent`
data SimpleEvt = SimpleEvt
  { time      :: Int -- Int for Ticks 
  , keynum    :: Int -- midi pitch
  , duration  :: Int -- Int for Ticks 
  , amplitude :: Int
--  , channel   :: Int
  }
  deriving (Eq,Show)
  
renderMidi :: [Pitch] -> MidiEnv -> MidiFile
renderMidi evts menv = MidiFile header1 [track1]
  where
    header1 = Header MF0 1 (TPB 480)    
    ss = notesToSE evts 0.5
    track1 = Track $ evt_SetTempo : (toMidi ss)

performMidi :: MidiFile -> MidiEnv -> IO ()
performMidi out env = do
  putStrLn $ "Writing " ++ (output_file env) ++ "..."
  writeMidi (output_file env) out

notesToSE :: [Pitch] -> Float -> [SimpleEvt]
notesToSE evts amp = map mkSimpleEvt (zip evts zs)
  where 
    zs = [n * 0.5 | n <- [0..] ]
    mkSimpleEvt (n,start) = SimpleEvt 
      { time      = deltaTime start
      , keynum    = midiValue $ fromPitch n
      , duration  = 480
      , amplitude = conv amp
      } 

    conv _ = 60

type Ticks = Int
type Seconds = Float

deltaTime :: Seconds -> Ticks    
deltaTime time_in_secs = round $ time_in_secs * 480.0

toMidi :: [SimpleEvt] -> [Message]
toMidi xs = (duop $ simple xs) ++ [evt_EndOfTrack]


duop [] = []
duop xs = zipWith (\(st,evt) (st',_) -> (st-st', evt) ) xs (xxs xs)
  where
    xxs (x:xs) = x:x:xs
    
   
simple xs = sortBy fn $ foldr (\a acc -> (midiOn a : midiOff a : acc) ) [] xs
  where fn (a,_) (b,_) = compare a b


midiOn :: SimpleEvt -> Message
midiOn (SimpleEvt {time=start',keynum=note',amplitude=a}) 
  = (fromIntegral $ start', VoiceEvent $ NoteOn 0 (fromIntegral note') (fromIntegral a))

midiOff :: SimpleEvt -> Message
midiOff (SimpleEvt {time=start',duration=dur',keynum=note',amplitude=a}) 
  = (fromIntegral $ start' + dur', VoiceEvent $ NoteOff 0 (fromIntegral note') 0x7f)



evt_SetTempo   = (0, MetaEvent (SetTempo 1000000))
evt_EndOfTrack = (0, MetaEvent EndOfTrack)
    
    