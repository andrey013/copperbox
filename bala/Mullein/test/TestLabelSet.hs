

module TestLabelSet where

import Mullein.ArbitraryInstances
import Mullein.CoreTypes
import Mullein.LabelSet

import Test.QuickCheck


prop_Seven_pitches :: Key -> Bool
prop_Seven_pitches = maybe True ((==7) . length . labels) . makeLabelSet
  

main :: IO ()
main = quickCheck prop_Seven_pitches

