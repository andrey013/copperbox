

-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- :set -i..

module Main where

import Bala.Format.Midi.Midi
import Bala.Base.Base

import qualified Bala.Perform.PerformPitch as PP

import Bala.Format.Midi.PerformMidi

bars1_4 :: [Pitch]
bars1_4 = decouperL  $
      "A4 B4 C#4 C#4 C#4 A4 C#4 C#4"
  ++ " C#4 A4 B4 C#4 B4 A4 A4" -- rest
  ++ " E4 D4 C#4 B4 C#4 A4 B4 C#4" 
  ++ " A4 B4 B4 A4 A4" -- rest

env =  default_env {
  output_file = "bulgarian6.midi" 
  }
  
main = output bars1_4 env  
  
  
  
  
   