
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.NamedElems
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Named elements
-- |
--------------------------------------------------------------------------------

module Bala.Base.NamedElems where

import Bala.Base.BaseExtra
import Bala.Base.PitchRep
import Bala.Base.Interval
import Bala.Base.AffiDecoInstances
import Bala.Base.Scale


middle_c :: Pitch
middle_c = decouper "C4"

c4,d4,e4,f4,g4,a4,b4 :: Pitch
c4 = decouper "C4"
d4 = decouper "D4"
e4 = decouper "E4"
f4 = decouper "F4"
g4 = decouper "G4"
a4 = decouper "A4"
b4 = decouper "B4"

c3,d3,e3,f3,g3,a3,b3 :: Pitch
c3 = decouper "C3"
d3 = decouper "D3"
e3 = decouper "E3"
f3 = decouper "F3"
g3 = decouper "G3"
a3 = decouper "A3"
b3 = decouper "B3"

c5,d5,e5,f5,g5,a5,b5 :: Pitch
c5 = decouper "C5"
d5 = decouper "D5"
e5 = decouper "E5"
f5 = decouper "F5"
g5 = decouper "G5"
a5 = decouper "A5"
b5 = decouper "B5"


major_intervals :: IntervalStructure
major_intervals             = mkIS "WWHWWWH"   -- bilaval
mixolydian_intervals        = mkIS "WWHWWHW"   -- khamaj
dorian_intervals            = mkIS "WHWWWHW"   -- kafi
aeolian_intervals           = mkIS "WHWWHWW"   -- asavari
phrygian_intervals          = mkIS "HWWWHWW"   -- bhairavi
lydian_intervals            = mkIS "WWWHWWH"   -- kaylan
todi_intervals              = mkIS "HWA2HHA2H"
purvi_intervals             = mkIS "HA2WHHA2H"
marwa_intervals             = mkIS "HA2WHWWH"
bhairav_intervals           = mkIS "HA2HWHA2H"
pentatonic_major_intervals  = mkIS "WWA2WA2"
pentatonic_minor_intervals  = mkIS "WWA2WA2"
chromatic_intervals         = mkIS "HHHHHHHHHHHH"

c_pentatonic_major = makeScale c4 pentatonic_major_intervals

perfect_unison    = interval 1 0
perfect_fourth    = interval 4 5
perfect_fifth     = interval 5 7
perfect_octave    = interval 8 12

major_second      = interval 2 2
major_third       = interval 3 4
major_sixth       = interval 6 9
major_seventh     = interval 7 11

minor_second      = interval 2 1
minor_third       = interval 3 3
minor_sixth       = interval 6 8
minor_seventh     = interval 7 10 

diminished_third  = interval 3 2
diminished_fifth  = interval 5 6

augmented_third   = interval 3 5 
augmented_fourth  = interval 4 6
augmented_fifth   = interval 5 8