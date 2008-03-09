
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

import Bala.Base.PitchRep
import Bala.Base.Scale


middle_c :: Pitch
middle_c = read "C4"

c4,d4,e4,f4,g4,a4,b4 :: Pitch
c4 = read "C4"
d4 = read "D4"
e4 = read "E4"
f4 = read "F4"
g4 = read "G4"
a4 = read "A4"
b4 = read "B4"

c3,d3,e3,f3,g3,a3,b3 :: Pitch
c3 = read "C3"
d3 = read "D3"
e3 = read "E3"
f3 = read "F3"
g3 = read "G3"
a3 = read "A3"
b3 = read "B3"

c5,d5,e5,f5,g5,a5,b5 :: Pitch
c5 = read "C5"
d5 = read "D5"
e5 = read "E5"
f5 = read "F5"
g5 = read "G5"
a5 = read "A5"
b5 = read "B5"


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

c_pentatonic_major = makeScale (read "C4") pentatonic_major_intervals

