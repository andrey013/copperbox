
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- :set -i../..

module Bulgarian6 where

import Sound.Bala.Format.Midi.MidiFile

import Sound.Bala.Base.Base

import Sound.Bala.Format.Midi.PerformMidi

bars1_4 :: [Pitch]
bars1_4 = elements $
     " A4 B4 C#4 C#4 C#4 A4 C#4 C#4"
  ++ " C#4 A4 B4 C#4 B4 A4 A4" -- rest
  ++ " E4 D4 C#4 B4 C#4 A4 B4 C#4" 
  ++ " A4 B4 B4 A4 A4" -- rest

env =  default_env {
  output_file = "bulgarian6.mid" 
  }
  
main = output bars1_4 env  
  
  
  
  
   