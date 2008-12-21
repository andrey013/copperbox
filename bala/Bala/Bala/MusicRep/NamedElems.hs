{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.MusicRep.NamedElems
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Named elements
--
--------------------------------------------------------------------------------

module Bala.MusicRep.NamedElems where

import Bala.Base.BaseExtra
import Bala.Base.Pitch

import Bala.MusicRep.Interval
import Bala.MusicRep.Scale


w = whole_step
h = half_step

major_interval_pattern :: IntervalPattern
major_interval_pattern = buildIntervalPattern major_intervals

major_intervals :: IntervalS
major_intervals = w . w . h . w . w . w . h 


dorian_interval_pattern :: IntervalPattern
dorian_interval_pattern = buildIntervalPattern major_intervals

dorian_intervals :: IntervalS
dorian_intervals = w . h . w . w . w . h . w 

{-


major_intervals, mixolydian_intervals, dorian_intervals, aeolian_intervals,
                 phrygian_intervals, lydian_intervals
    :: IntervalPattern
major_intervals             = decouper "WWHWWWH"   -- bilaval
mixolydian_intervals        = decouper "WWHWWHW"   -- khamaj
dorian_intervals            = decouper "WHWWWHW"   -- kafi
aeolian_intervals           = decouper "WHWWHWW"   -- asavari
phrygian_intervals          = decouper "HWWWHWW"   -- bhairavi
lydian_intervals            = decouper "WWWHWWH"   -- kaylan

todi_intervals, purvi_intervals, marwa_intervals, bhairav_intervals
   :: IntervalPattern
todi_intervals              = decouper "HWA2HHA2H"
purvi_intervals             = decouper "HA2WHHA2H"
marwa_intervals             = decouper "HA2WHWWH"
bhairav_intervals           = decouper "HA2HWHA2H"


-- | Scale pattern M2M2m3M2m3
pentatonic_major_intervals :: IntervalPattern
pentatonic_major_intervals  = decouper "M2M2m3M2m3"


-- | Scale pattern m3M2M2m3M2
pentatonic_minor_intervals :: IntervalPattern
pentatonic_minor_intervals  = decouper "m3M2M2m3M2"



-- | The interval pattern of 12 half steps HHHHHHHHHHHH
chromatic_intervals :: IntervalPattern 
chromatic_intervals         = decouper "HHHHHHHHHHHH"

-- c_pentatonic_major :: Scale
-- c_pentatonic_major = makeScale c4 pentatonic_major_intervals


-}

