

module TestLabelSet where

import Mullein.ArbitraryInstances
import Mullein.CoreTypes
import Mullein.Pitch
import Mullein.LabelSet

import Test.QuickCheck


prop_Seven_pitches :: (Key,[PitchLabel]) -> Bool
prop_Seven_pitches (k,xs) = maybe True ((==7) . length . labels) $  makeLabelSet k xs
  

main :: IO ()
main = quickCheck prop_Seven_pitches


-- This LabelSet only has 6 pitches (E## is F#, so F# overides E## in the Map)
--
-- ... Looks like LabelSet isn't the right data structure for the job
demo = makeLabelSet (Key (PitchLabel E Nat) Lydian) [PitchLabel E DoubleSharp]

