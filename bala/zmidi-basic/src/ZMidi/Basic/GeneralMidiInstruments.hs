{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.GeneralMidiInstruments
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Datatypes for general MIDI instruments.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.GeneralMidiInstruments
  (
  -- * General MIDI instruments
    GMInst(..)

  -- * General MIDI drums
  , GMDrum(..)

  , drumPitch

  ) where

import Data.Word

-- | Enumeration of the General MIDI instruments.
data GMInst
    -- | Piano
    = Acoustic_grand_piano
    | Bright_acoustic_piano
    | Electric_grand_piano
    | Honky_tonk
    | Electric_piano_1
    | Electric_piano_2
    | Harpsichord
    | Clavicord

    -- | Chromatic persussion
    | Celesta
    | Glockenspiel
    | Music_box
    | Vibraphone
    | Marimba
    | Xylophone
    | Tubular_bells
    | Dulcimmer

    -- | Organ
    | Drawbar_organ
    | Percussive_organ
    | Rock_organ
    | Church_organ
    | Reel_organ
    | Accordian
    | Harmonica
    | Tango_accordian

    -- | Guitar
    | Nylon_acoustic_guitar
    | Steel_acoustic_guitar
    | Jazz_electric_guitar
    | Clean_electric_guitar
    | Muted_electric_guitar
    | Overdriven_guitar
    | Distortion_guitar
    | Guitar_harmonics

    -- | Bass
    | Acoustic_bass
    | Finger_electric_bass
    | Pick_electric_bass
    | Fretless_bass
    | Slap_bass_1
    | Slap_bass_2
    | Synth_bass_1
    | Synth_bass_2

    -- | Strings
    | Violin
    | Viola
    | Cello
    | Contrabass
    | Tremolo_strings
    | Pizzicato_strings
    | Orchestral_strings
    | Timpani

    -- | Ensemble
    | String_ensemble_1
    | String_ensemble_2
    | Synth_strings_1
    | Synth_strings_2
    | Choir_aahs
    | Voice_oohs
    | Synth_voice
    | Orchestra_hit

    -- | Brass
    | Trumpet
    | Trombone
    | Tuba
    | Muted_trompet
    | French_horn
    | Brass_section
    | Synth_brass_1
    | Synth_brass_2

    -- |  Reed
    | Soprano_sax
    | Alto_sax
    | Tenor_sax
    | Baritone_sax
    | Oboe
    | English_horn
    | Bassoon
    | Clarinet

    -- | Pipe
    | Piccolo
    | Flute
    | Recorder
    | Pan_flute
    | Blown_bottle
    | Shakuhachi
    | Whistle
    | Ocarina

    -- | Synth lead
    | Square_lead
    | Sawtooth_lead
    | Calliope_lead
    | Chiff_lead
    | Charang_lead
    | Voice_lead
    | Fifths_lead
    | Bass_lead

    -- | Synth pad
    | New_age_pad
    | Warm_pad
    | Polysynth_pad
    | Choir_pad
    | Bowed_pad
    | Metallic_pad
    | Halo_pad
    | Sweep_pad

    -- | Synth effects
    | Rain
    | Soundtrack
    | Crystal
    | Atmosphere
    | Brightness
    | Goblins
    | Echoes
    | Sci_fi

    -- | World
    | Sitar
    | Banjo
    | Shamisen
    | Koto
    | Kalimba
    | Bagpipe
    | Fiddle
    | Shanai

    -- | Percussive
    | Tingle_bell
    | Agogo
    | Steel_drums
    | Woodblock
    | Taiko_drum
    | Melodic_tom
    | Synth_drum
    | Reverse_cymbal

    -- | Sound effects
    | Guitar_fret_noise
    | Breath_noise
    | Seashore
    | Bird_tweet
    | Telephone_ring
    | Helicopter
    | Applause
    | Gunshot
  deriving (Eq,Ord,Enum,Read,Show)

-- | Enumeration of the General MIDI drum types.
data GMDrum
    = Acoustic_bass_drum
    | Bass_drum_1
    | Side_stick
    | Acoustic_snare
    | Hand_clap
    | Electric_snare
    | Low_floor_tom
    | Closed_hi_hat
    | High_floor_tom
    | Pedal_hi_hat
    | Low_tom
    | Open_hi_hat
    | Low_mid_tom
    | High_mid_tom
    | Crash_cymbal_1
    | High_tom
    | Ride_cymbal_1
    | Chinese_cymbal
    | Ride_bell
    | Tambourine
    | Splash_cymbal
    | Cowbell
    | Crash_cymbal_2
    | Vibraslap
    | Ride_cymbal_2
    | High_bongo
    | Low_bongo
    | Mute_high_conga
    | Open_high_conga
    | Low_conga
    | High_timbale
    | Low_timbale
    | High_agogo
    | Low_agogo
    | Cabasa
    | Maracas
    | Short_whistle
    | Long_whistle
    | Short_guiro
    | Long_guiro
    | Claves
    | High_wood_block
    | Low_wood_block
    | Mute_cuica
    | Open_cuica
    | Mute_triangle
    | Open_triangle
  deriving (Eq,Ord,Enum,Read,Show)

-- General Midi drums are in the range [35..81]
drumPitch :: GMDrum -> Word8
drumPitch = (+35) . fromIntegral . fromEnum


