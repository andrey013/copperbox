


module TestPitchConversion 
  ( testPitchConversion
  )where

import ArbitraryInstances
import Bala

import Test.QuickCheck

testPitchConversion = mapM_ quickCheck [prop_Identity]

middleC :: MidiPitch
middleC = fromPitch (read "C4")

prop_Identity :: MidiPitch -> Bool
prop_Identity a = a ==  (hzMidi $ midiHz a)

