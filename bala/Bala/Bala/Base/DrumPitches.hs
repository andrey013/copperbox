
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.DrumPitches
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- DrumPitches - representing drums as pitches that can be interpreted 
-- by Midi.
--
--------------------------------------------------------------------------------


module Bala.Base.DrumPitches where

import Bala.Base.Duration
import Bala.Base.Pitch

import HNotate.NoteListDatatypes (Tile)
import qualified HNotate.Marks as H 


acoustic_bass_drum      :: Pitch
acoustic_bass_drum      = fromSemitones 35 

bass_drum_1             :: Pitch
bass_drum_1             = fromSemitones 36

side_stick              :: Pitch
side_stick              = fromSemitones 37 

acoustic_snare          :: Pitch
acoustic_snare          = fromSemitones 38

hand_clap               :: Pitch
hand_clap               = fromSemitones 39

electric_snare          :: Pitch
electric_snare          = fromSemitones 40

low_floor_tom           :: Pitch
low_floor_tom           = fromSemitones 41

closed_hi_hat           :: Pitch
closed_hi_hat           = fromSemitones 42

high_floor_tom          :: Pitch
high_floor_tom          = fromSemitones 43

pedal_hi_hat            :: Pitch
pedal_hi_hat            = fromSemitones 44

low_tom                 :: Pitch
low_tom                 = fromSemitones 45

open_hi_hat             :: Pitch
open_hi_hat             = fromSemitones 46

low_mid_tom             :: Pitch
low_mid_tom             = fromSemitones 47

high_mid_tom            :: Pitch
high_mid_tom            = fromSemitones 48

crash_cymbal_1          :: Pitch
crash_cymbal_1          = fromSemitones 49

high_tom                :: Pitch
high_tom                = fromSemitones 50

ride_cymbal_1           :: Pitch
ride_cymbal_1           = fromSemitones 51

chinese_cymbal          :: Pitch
chinese_cymbal          = fromSemitones 52

ride_bell               :: Pitch
ride_bell               = fromSemitones 53

tambourine              :: Pitch
tambourine              = fromSemitones 54

splash_cymbal           :: Pitch
splash_cymbal           = fromSemitones 55

cowbell                 :: Pitch
cowbell                 = fromSemitones 56

crash_cymbal_2          :: Pitch
crash_cymbal_2          = fromSemitones 57

vibraslap               :: Pitch
vibraslap               = fromSemitones 58

ride_cymbal_2           :: Pitch
ride_cymbal_2           = fromSemitones 59

high_bongo              :: Pitch
high_bongo              = fromSemitones 60

low_bongo               :: Pitch
low_bongo               = fromSemitones 61

mute_high_conga         :: Pitch
mute_high_conga         = fromSemitones 62

open_high_conga         :: Pitch
open_high_conga         = fromSemitones 63

low_conga               :: Pitch
low_conga               = fromSemitones 64

high_timbale            :: Pitch
high_timbale            = fromSemitones 65

low_timbale             :: Pitch
low_timbale             = fromSemitones 66

high_agogo              :: Pitch
high_agogo              = fromSemitones 67

low_agogo               :: Pitch
low_agogo               = fromSemitones 68

cabasa                  :: Pitch
cabasa                  = fromSemitones 69

maracas                 :: Pitch
maracas                 = fromSemitones 70

short_whistle           :: Pitch
short_whistle           = fromSemitones 71

long_whistle            :: Pitch
long_whistle            = fromSemitones 72

short_guiro             :: Pitch
short_guiro             = fromSemitones 73

long_guiro              :: Pitch
long_guiro              = fromSemitones 74

claves                  :: Pitch
claves                  = fromSemitones 75

high_wood_block         :: Pitch
high_wood_block         = fromSemitones 76

low_wood_block          :: Pitch
low_wood_block          = fromSemitones 77

mute_cuica              :: Pitch
mute_cuica              = fromSemitones 78

open_cuica              :: Pitch
open_cuica              = fromSemitones 79

mute_triangle           :: Pitch
mute_triangle           = fromSemitones 80

open_triangle           :: Pitch
open_triangle           = fromSemitones 81

-- LilyPond has more drums than General Midi (?)
drumEvent :: Pitch -> Duration -> Tile
drumEvent p d 
    | p == acoustic_bass_drum     = H.acousticbassdrum  d
    | p == bass_drum_1            = H.bassdrum          d
    | p == side_stick             = H.sidestick         d
    | p == acoustic_snare         = H.acousticsnare     d
    | p == hand_clap              = H.handclap          d
    | p == electric_snare         = H.electricsnare     d
    | p == low_floor_tom          = H.lowfloortom       d
    | p == closed_hi_hat          = H.closedhihat       d
    | p == high_floor_tom         = H.highfloortom      d
    | p == pedal_hi_hat           = H.pedalhihat        d
    | p == low_tom                = H.lowtom            d
    | p == open_hi_hat            = H.openhihat         d
    | p == low_mid_tom            = H.lowmidtom         d
    | p == high_mid_tom           = H.himidtom          d
    | p == crash_cymbal_1         = H.crashcymbala      d
    | p == high_tom               = H.hightom           d
    | p == ride_cymbal_1          = H.ridecymbala       d
    | p == chinese_cymbal         = H.chinesecymbal     d
    | p == ride_bell              = H.ridebell          d
    | p == tambourine             = H.tambourine        d
    | p == splash_cymbal          = H.splashcymbal      d
    | p == cowbell                = H.cowbell           d
    | p == crash_cymbal_2         = H.crashcymbalb      d
    | p == vibraslap              = H.vibraslap         d
    | p == ride_cymbal_2          = H.ridecymbalb       d
    | p == high_bongo             = H.hibongo           d
    | p == low_bongo              = H.lobongo           d
    | p == mute_high_conga        = H.mutehiconga       d
    | p == open_high_conga        = H.openhiconga       d
    | p == low_conga              = H.loconga           d
    | p == high_timbale           = H.hitimbale         d
    | p == low_timbale            = H.lotimbale         d
    | p == high_agogo             = H.hiagogo           d
    | p == low_agogo              = H.loagogo           d
    | p == cabasa                 = H.cabasa            d
    | p == maracas                = H.maracas           d
    | p == short_whistle          = H.shortwhistle      d
    | p == long_whistle           = H.longwhistle       d
    | p == short_guiro            = H.shortguiro        d
    | p == long_guiro             = H.longguiro         d
    | p == claves                 = H.claves            d
    | p == high_wood_block        = H.hiwoodblock       d
    | p == low_wood_block         = H.lowoodblock       d
    | p == mute_cuica             = H.mutecuica         d
    | p == open_cuica             = H.opencuica         d
    | p == mute_triangle          = H.mutetriangle      d
    | p == open_triangle          = H.opentriangle      d
    | otherwise                   = H.oneup             d     -- arbitrary...

    
    
   