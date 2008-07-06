

-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- :set -i..

module Main where

import Bala.Format.Midi.Midi
import Bala.Base.Base

import Bala.Perform.RenderMidi
import qualified Bala.Perform.EventTree as E
import Bala.Perform.EventTree ( (#) )

pitches_bars1_4 :: [Pitch]
pitches_bars1_4 = decouperL  $
      "A4 B4 C#4 C#4 C#4 A4 C#4 C#4"
  ++ " C#4 A4 B4 C#4 B4 A4 A4" -- rest
  ++ " E4 D4 C#4 B4 C#4 A4 B4 C#4" 
  ++ " A4 B4 B4 A4 A4" -- rest

bars1_4 :: E.EventTree (Pitch, Duration)
bars1_4 = foldl fn E.root pitches_bars1_4
  where
    fn tree p = tree # E.note (p,quarter) 

bulgarian6 = (E.Perf [bars1_4])   
  
main = output bulgarian6 "bulgarian6.mid"

  
  
  
  
  
   