{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit.Constructors
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Constructors for pitches, durations, General MIDI instruments,
-- General MIDI Drums.
--
--------------------------------------------------------------------------------

module ZMidi.Emit.Constructors
  (

  -- * Type aliases
    MidiPitch
  , MidiDuration

  -- * Pitch constructors 
  , PitchCons

  , c_nat  
  , c_sharp
  , d_flat
  , d_nat
  , d_sharp
  , e_flat
  , e_nat
  , f_nat
  , f_sharp
  , g_flat
  , g_nat
  , g_sharp
  , a_flat
  , a_nat
  , b_flat
  , b_nat

  -- * Duration constructors
  , dwhole
  , dhalf
  , dquarter
  , deighth
  , dsixteenth

  -- * General MIDI instruments
  , GMInst

  , acoustic_grand_piano
  , bright_acoustic_piano
  , electric_grand_piano
  , honky_tonk
  , electric_piano_1
  , electric_piano_2
  , harpsichord
  , clavicord

    -- ** Chromatic persussion
  , celesta
  , glockenspiel
  , music_box
  , vibraphone
  , marimba
  , xylophone
  , tubular_bells
  , dulcimer

  -- ** Organ
  , drawbar_organ
  , percussive_organ
  , rock_organ
  , church_organ
  , reel_organ
  , accordian
  , harmonica
  , tango_accordian

  -- ** Guitar
  , nylon_acoustic_guitar
  , steel_acoustic_guitar
  , jazz_electric_guitar
  , clean_electric_guitar
  , muted_electric_guitar
  , overdriven_guitar
  , distortion_guitar
  , guitar_harmonics

  -- ** Bass
  , acoustic_bass
  , finger_electric_bass
  , pick_electric_bass
  , fretless_bass
  , slap_bass_1
  , slap_bass_2
  , synth_bass_1
  , synth_bass_2

  -- ** Strings
  , violin
  , viola
  , cello
  , contrabass
  , tremolo_strings
  , pizzicato_strings
  , orchestral_strings
  , timpani

  -- ** Ensemble
  , string_ensemble_1
  , string_ensemble_2
  , synth_strings_1
  , synth_strings_2
  , choir_aahs
  , voice_oohs
  , synth_voice
  , orchestra_hit

  -- ** Brass
  , trumpet
  , trombone
  , tuba
  , muted_trumpet
  , french_horn
  , brass_section
  , synth_brass_1
  , synth_brass_2

  -- ** Reed
  , soprano_sax
  , alto_sax
  , tenor_sax
  , baritone_sax
  , oboe
  , english_horn
  , bassoon
  , clarinet

  -- ** Pipe
  , piccolo
  , flute
  , recorder
  , pan_flute
  , blown_bottle
  , shakuhachi
  , whistle
  , ocarina

  -- ** Synth lead
  , square_lead
  , sawtooth_lead
  , calliope_lead
  , chiff_lead
  , charang_lead
  , voice_lead
  , fifths_lead
  , bass_lead

  -- ** Synth pad
  , new_age_pad
  , warm_pad
  , polysynth_pad
  , choir_pad
  , bowed_pad
  , metallic_pad
  , halo_pad
  , sweep_pad

  -- ** Synth effects
  , rain
  , soundtrack
  , crystal
  , atmosphere
  , brightness
  , goblins
  , echoes
  , sci_fi

  -- ** World
  , sitar
  , banjo
  , shamisen
  , koto
  , kalimba
  , bagpipe
  , fiddle
  , shanai

  -- ** Percussive
  , tingle_bell
  , agogo
  , steel_drums
  , woodblock
  , taiko_drum
  , melodic_tom
  , synth_drum
  , reverse_cymbal


  -- ** Sound effects
  , guitar_fret_noise
  , breath_noise
  , seashore
  , bird_tweet
  , telephone_ring
  , helicopter
  , applause
  , gunshot


  -- * General MIDI drums
  , GMDrum
  , acoustic_bass_drum
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

import Data.Word


-- | 'MidiPitch' is a Word8, corresponding directly to the MIDI
-- representation, only values in the range @[0..127]@ are
-- allowed.
--
-- Middle c is 60. Each increment is a semitone step. With careful
-- use of the pitch-bend control signal MIDI can simulate
-- microtonal intervals though this is not attempted by
-- @ZMidi.Emit@.
--
type MidiPitch    = Word8

-- | 'MidiDuration' is a Double, directly corresponding to the
-- dureation value:
--
-- @1.0@ represents a whole note.
--
-- @0.5@ represents a half note.
--
-- @0.25@ represents a half note. Etc.
--
-- Using Double allows some cleverness for representing special
-- durations, e.g. grace notes can be some small duration
-- subtracted from the note next to the grace.
--
-- Internally @ZMidi.Emit@ translates the Double value into and
-- integer number of ticks.
--
type MidiDuration = Double


--------------------------------------------------------------------------------

clampPch :: Int -> Word8
clampPch i | i < 0     = 0
           | i > 127   = 127
           | otherwise = fromIntegral i

-- | A Pitch constructor takes and Octave and builds a MidiPitch.
--
-- Octave is an integer value - middle C is octave 4.
--
-- An octave spans from C to B.
-- 
type PitchCons = Int -> MidiPitch

c_nat       :: PitchCons
c_nat o     = clampPch $     12 * (o + 1)

c_sharp     :: PitchCons
c_sharp o   = clampPch $ 1 + 12 * (o + 1)

d_flat      :: PitchCons
d_flat      = c_sharp

d_nat       :: PitchCons
d_nat o     = clampPch $ 2 + 12 * (o + 1)

d_sharp     :: PitchCons
d_sharp o   = clampPch $ 3 + 12 * (o + 1)

e_flat      :: PitchCons
e_flat      = d_sharp

e_nat       :: PitchCons
e_nat o     = clampPch $ 4 + 12 * (o + 1)

f_nat       :: PitchCons
f_nat o     = clampPch $ 5 + 12 * (o + 1)

f_sharp     :: PitchCons
f_sharp o   = clampPch $ 6 + 12 * (o + 1)

g_flat      :: PitchCons
g_flat      = f_sharp

g_nat       :: PitchCons
g_nat o     = clampPch $ 7 + 12 * (o + 1)

g_sharp     :: PitchCons
g_sharp o   = clampPch $ 8 + 12 * (o + 1)

a_flat      :: PitchCons
a_flat     = a_sharp

a_nat       :: PitchCons
a_nat o     = clampPch $ 9 + 12 * (o + 1)

a_sharp     :: PitchCons
a_sharp o   = clampPch $ 10 + 12 * (o + 1)

b_flat      :: PitchCons
b_flat      = a_sharp

b_nat       :: PitchCons
b_nat o     = clampPch $ 11 + 12 * (o + 1)


-- | A whole beat.
--
-- Value @1.0@.
--
dwhole      :: MidiDuration
dwhole      = 1.0

-- | A half beat.
-- 
-- Value @0.5@.
--
dhalf       :: MidiDuration
dhalf       = 0.5

-- | A quarter beat.
-- 
-- Value @0.25@.
--
dquarter    :: MidiDuration
dquarter    = 0.25

-- | A eighth beat.
-- 
-- Value @0.125@.
--
deighth     :: MidiDuration
deighth     = 0.125


-- | A sixteenth beat.
-- 
-- Value @0.0625@.
--
dsixteenth  :: MidiDuration
dsixteenth   = 0.0625


--------------------------------------------------------------------------------

-- | Enumeration of the General MIDI instruments.
--
type GMInst = Word8

    -- | Piano
acoustic_grand_piano    :: GMInst
acoustic_grand_piano    = 0

bright_acoustic_piano   :: GMInst
bright_acoustic_piano   = 1

electric_grand_piano    :: GMInst
electric_grand_piano    = 2

honky_tonk              :: GMInst
honky_tonk              = 3

electric_piano_1        :: GMInst
electric_piano_1        = 4

electric_piano_2        :: GMInst
electric_piano_2        = 5

harpsichord             :: GMInst
harpsichord             = 6

clavicord               :: GMInst
clavicord               = 7


-- | Chromatic persussion

celesta                 :: GMInst
celesta                 = 8

glockenspiel            :: GMInst
glockenspiel            = 9

music_box               :: GMInst
music_box               = 10

vibraphone              :: GMInst
vibraphone              = 11

marimba                 :: GMInst
marimba                 = 12

xylophone               :: GMInst
xylophone               = 13

tubular_bells           :: GMInst
tubular_bells           = 14

dulcimer                :: GMInst
dulcimer                = 15


-- | Organ

drawbar_organ           :: GMInst
drawbar_organ           = 16

percussive_organ        :: GMInst
percussive_organ        = 17

rock_organ              :: GMInst
rock_organ              = 18

church_organ            :: GMInst
church_organ            = 19

reel_organ              :: GMInst
reel_organ              = 20

accordian               :: GMInst
accordian               = 21

harmonica               :: GMInst
harmonica               = 22

tango_accordian         :: GMInst
tango_accordian         = 23

-- | Guitar
nylon_acoustic_guitar   :: GMInst
nylon_acoustic_guitar   = 24

steel_acoustic_guitar   :: GMInst
steel_acoustic_guitar   = 25

jazz_electric_guitar    :: GMInst
jazz_electric_guitar    = 26

clean_electric_guitar   :: GMInst
clean_electric_guitar   = 27

muted_electric_guitar   :: GMInst
muted_electric_guitar   = 28

overdriven_guitar       :: GMInst
overdriven_guitar       = 29

distortion_guitar       :: GMInst
distortion_guitar       = 30

guitar_harmonics        :: GMInst
guitar_harmonics        = 31


-- | Bass
acoustic_bass           :: GMInst
acoustic_bass           = 32

finger_electric_bass    :: GMInst
finger_electric_bass    = 33

pick_electric_bass      :: GMInst
pick_electric_bass      = 34

fretless_bass           :: GMInst
fretless_bass           = 35

slap_bass_1             :: GMInst
slap_bass_1             = 36
                        
slap_bass_2             :: GMInst
slap_bass_2             = 37

synth_bass_1            :: GMInst
synth_bass_1            = 38

synth_bass_2            :: GMInst
synth_bass_2            = 39

-- | Strings
violin                  :: GMInst
violin                  = 40

viola                   :: GMInst
viola                   = 41

cello                   :: GMInst
cello                   = 42

contrabass              :: GMInst
contrabass              = 43

tremolo_strings         :: GMInst
tremolo_strings         = 44

pizzicato_strings       :: GMInst
pizzicato_strings       = 45

orchestral_strings      :: GMInst
orchestral_strings      = 46

timpani                 :: GMInst
timpani                 = 47


-- | Ensemble
string_ensemble_1       :: GMInst
string_ensemble_1       = 48

string_ensemble_2       :: GMInst
string_ensemble_2       = 49

synth_strings_1         :: GMInst
synth_strings_1         = 50

synth_strings_2         :: GMInst
synth_strings_2         = 51

choir_aahs              :: GMInst
choir_aahs              = 52

voice_oohs              :: GMInst
voice_oohs              = 53

synth_voice             :: GMInst
synth_voice             = 54

orchestra_hit           :: GMInst
orchestra_hit           = 55


-- | Brass
trumpet                 :: GMInst
trumpet                 = 56

trombone                :: GMInst
trombone                = 57

tuba                    :: GMInst
tuba                    = 58

muted_trumpet           :: GMInst
muted_trumpet           = 59

french_horn             :: GMInst
french_horn             = 60

brass_section           :: GMInst
brass_section           = 61

synth_brass_1           :: GMInst
synth_brass_1           = 62

synth_brass_2           :: GMInst
synth_brass_2           = 63


-- |  Reed
soprano_sax             :: GMInst
soprano_sax             = 64

alto_sax                :: GMInst
alto_sax                = 65

tenor_sax               :: GMInst
tenor_sax               = 66

baritone_sax            :: GMInst
baritone_sax            = 67

oboe                    :: GMInst
oboe                    = 68

english_horn            :: GMInst
english_horn            = 69

bassoon                 :: GMInst
bassoon                 = 70

clarinet                :: GMInst
clarinet                = 71

-- | Pipe
piccolo                 :: GMInst
piccolo                 = 72

flute                   :: GMInst
flute                   = 73

recorder                :: GMInst
recorder                = 74

pan_flute               :: GMInst
pan_flute               = 75

blown_bottle            :: GMInst
blown_bottle            = 76

shakuhachi              :: GMInst
shakuhachi              = 77

whistle                 :: GMInst
whistle                 = 78

ocarina                 :: GMInst
ocarina                 = 79

-- | Synth lead
square_lead             :: GMInst
square_lead             = 80

sawtooth_lead           :: GMInst
sawtooth_lead           = 81

calliope_lead           :: GMInst
calliope_lead           = 82

chiff_lead              :: GMInst
chiff_lead              = 83

charang_lead            :: GMInst
charang_lead            = 84

voice_lead              :: GMInst
voice_lead              = 85

fifths_lead             :: GMInst
fifths_lead             = 86

bass_lead               :: GMInst
bass_lead               = 87

-- | Synth pad
new_age_pad             :: GMInst
new_age_pad             = 88

warm_pad                :: GMInst
warm_pad                = 89

polysynth_pad           :: GMInst
polysynth_pad           = 90

choir_pad               :: GMInst
choir_pad               = 91

bowed_pad               :: GMInst
bowed_pad               = 92

metallic_pad            :: GMInst
metallic_pad            = 93

halo_pad                :: GMInst
halo_pad                = 94

sweep_pad               :: GMInst
sweep_pad               = 95

-- | Synth effects
rain                    :: GMInst
rain                    = 96

soundtrack              :: GMInst
soundtrack              = 97

crystal                 :: GMInst
crystal                 = 98

atmosphere              :: GMInst
atmosphere              = 99

brightness              :: GMInst
brightness              = 100

goblins                 :: GMInst
goblins                 = 101

echoes                  :: GMInst
echoes                  = 102

sci_fi                  :: GMInst
sci_fi                  = 103

-- | World
sitar                   :: GMInst
sitar                   = 104

banjo                   :: GMInst
banjo                   = 105

shamisen                :: GMInst
shamisen                = 106

koto                    :: GMInst
koto                    = 107

kalimba                 :: GMInst
kalimba                 = 108

bagpipe                 :: GMInst
bagpipe                 = 109

fiddle                  :: GMInst
fiddle                  = 110

shanai                  :: GMInst
shanai                  = 111

-- | Percussive
tingle_bell             :: GMInst
tingle_bell             = 112

agogo                   :: GMInst
agogo                   = 113

steel_drums             :: GMInst
steel_drums             = 114

woodblock               :: GMInst
woodblock               = 115

taiko_drum              :: GMInst
taiko_drum              = 116

melodic_tom             :: GMInst
melodic_tom             = 117

synth_drum              :: GMInst
synth_drum              = 118

reverse_cymbal          :: GMInst
reverse_cymbal          = 119


-- | Sound effects
guitar_fret_noise       :: GMInst
guitar_fret_noise       = 120

breath_noise            :: GMInst
breath_noise            = 121

seashore                :: GMInst
seashore                = 122

bird_tweet              :: GMInst
bird_tweet              = 123

telephone_ring          :: GMInst
telephone_ring          = 124

helicopter              :: GMInst
helicopter              = 125

applause                :: GMInst
applause                = 126

gunshot                 :: GMInst
gunshot                 = 127


-------------------------------------------------------------------------------

-- | Enumeration of the General MIDI drum types.
--
type GMDrum = Word8

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

