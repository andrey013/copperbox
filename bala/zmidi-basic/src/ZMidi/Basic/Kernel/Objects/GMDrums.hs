{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.GMDrums
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Named instances of General MIDI drums.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.GMDrums
  (

  -- * General MIDI drums
    acoustic_bass_drum
  , bass_drum_1
  , side_stick
  , acoustic_snare
  , hand_clap
  , electric_snare
  , low_floor_tom
  , closed_hi_hat
  , high_floor_tom
  , pedal_hi_hat
  , low_tom
  , open_hi_hat
  , low_mid_tom
  , high_mid_tom
  , crash_cymbal_1
  , high_tom
  , ride_cymbal_1
  , chinese_cymbal
  , ride_bell
  , tambourine
  , splash_cymbal
  , cowbell
  , crash_cymbal_2
  , vibraslap
  , ride_cymbal_2
  , high_bongo
  , low_bongo
  , mute_high_conga
  , open_high_conga
  , low_conga
  , high_timbale
  , low_timbale
  , high_agogo
  , low_agogo
  , cabasa
  , maracas
  , short_whistle
  , long_whistle
  , short_guiro
  , long_guiro
  , claves
  , high_wood_block
  , low_wood_block
  , mute_cuica
  , open_cuica
  , mute_triangle
  , open_triangle

  ) where


import ZMidi.Basic.Kernel.Base.BaseDefs


-------------------------------------------------------------------------------
-- GM drums

acoustic_bass_drum      :: GMDrum
acoustic_bass_drum      = 35

bass_drum_1             :: GMDrum
bass_drum_1             = 36

side_stick              :: GMDrum
side_stick              = 37

acoustic_snare          :: GMDrum
acoustic_snare          = 38

hand_clap               :: GMDrum
hand_clap               = 39

electric_snare          :: GMDrum
electric_snare          = 40

low_floor_tom           :: GMDrum
low_floor_tom           = 41

closed_hi_hat           :: GMDrum
closed_hi_hat           = 42

high_floor_tom          :: GMDrum
high_floor_tom          = 43

pedal_hi_hat            :: GMDrum
pedal_hi_hat            = 44

low_tom                 :: GMDrum
low_tom                 = 45

open_hi_hat             :: GMDrum
open_hi_hat             = 46

low_mid_tom             :: GMDrum
low_mid_tom             = 47

high_mid_tom            :: GMDrum
high_mid_tom            = 48

crash_cymbal_1          :: GMDrum
crash_cymbal_1          = 49

high_tom                :: GMDrum
high_tom                = 50

ride_cymbal_1           :: GMDrum
ride_cymbal_1           = 51

chinese_cymbal          :: GMDrum
chinese_cymbal          = 52

ride_bell               :: GMDrum
ride_bell               = 53

tambourine              :: GMDrum
tambourine              = 54

splash_cymbal           :: GMDrum
splash_cymbal           = 55

cowbell                 :: GMDrum
cowbell                 = 56

crash_cymbal_2          :: GMDrum
crash_cymbal_2          = 57

vibraslap               :: GMDrum
vibraslap               = 58

ride_cymbal_2           :: GMDrum
ride_cymbal_2           = 59

high_bongo              :: GMDrum
high_bongo              = 60

low_bongo               :: GMDrum
low_bongo               = 61

mute_high_conga         :: GMDrum
mute_high_conga         = 62

open_high_conga         :: GMDrum
open_high_conga         = 63

low_conga               :: GMDrum
low_conga               = 64

high_timbale            :: GMDrum
high_timbale            = 65

low_timbale             :: GMDrum
low_timbale             = 66

high_agogo              :: GMDrum
high_agogo              = 67

low_agogo               :: GMDrum
low_agogo               = 68

cabasa                  :: GMDrum
cabasa                  = 69

maracas                 :: GMDrum
maracas                 = 70

short_whistle           :: GMDrum
short_whistle           = 71

long_whistle            :: GMDrum
long_whistle            = 72

short_guiro             :: GMDrum
short_guiro             = 73

long_guiro              :: GMDrum
long_guiro              = 74

claves                  :: GMDrum
claves                  = 75

high_wood_block         :: GMDrum
high_wood_block         = 76

low_wood_block          :: GMDrum
low_wood_block          = 77

mute_cuica              :: GMDrum
mute_cuica              = 78

open_cuica              :: GMDrum
open_cuica              = 79

mute_triangle           :: GMDrum
mute_triangle           = 80

open_triangle           :: GMDrum
open_triangle           = 81

