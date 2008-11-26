
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

import qualified HNotate.NoteListDatatypes as H
import qualified HNotate.Marks as H 
import qualified ZMidi as Z (GMDrum(..), drumPitch)


-- Just us a subset of /representable/ drums - the drums-style drums 
-- from LilyPond. 
-- Note unfortunately the order of the drums does not align with
-- the order of general midi drums.
data DrumsStyle = Acousticbassdrum
                | Bassdrum
                | Sidestick
                | Acousticsnare
                | Snare
                | Handclap
                | Electricsnare
                | Lowfloortom
                | Closedhihat
                | Hihat
                | Highfloortom
                | Pedalhihat
                | Lowtom
                | Openhihat
                | Halfopenhihat
                | Lowmidtom
                | Himidtom
                | Crashcymbala
                | Crashcymbal
                | Hightom
                | Ridecymbala
                | Ridecymbal
                | Chinesecymbal
                | Ridebell
                | Splashcymbal
                | Cowbell
                | Crashcymbalb
                | Vibraslap
                | Ridecymbalb
  deriving (Enum,Eq,Ord,Show)
  
gmDrumPitch :: Z.GMDrum -> Pitch
gmDrumPitch = fromSemitones . fromIntegral . Z.drumPitch

pitchToGmDrum :: Pitch -> Maybe Z.GMDrum
pitchToGmDrum = fn . semitones where
  fn i | i >= 35 && i <= 81   = Just $ toEnum $ i -35
       | otherwise            = Nothing
   

-- Not all drums have a direct mapping
-- id ___ marks the ones that don't
gmDrum :: DrumsStyle -> Z.GMDrum
gmDrum Acousticbassdrum     = Z.Acoustic_bass_drum
gmDrum Bassdrum             = Z.Bass_drum_1
gmDrum Sidestick            = Z.Side_stick
gmDrum Acousticsnare        = Z.Acoustic_snare
gmDrum Snare                = id                  Z.Short_guiro
gmDrum Handclap             = Z.Hand_clap  
gmDrum Electricsnare        = Z.Electric_snare
gmDrum Lowfloortom          = Z.Low_floor_tom
gmDrum Closedhihat          = Z.Closed_hi_hat
gmDrum Hihat                = id                  Z.Long_guiro
gmDrum Highfloortom         = Z.High_floor_tom
gmDrum Pedalhihat           = Z.Pedal_hi_hat
gmDrum Lowtom               = Z.Low_tom
gmDrum Openhihat            = Z.Open_hi_hat
gmDrum Halfopenhihat        = id                  Z.Claves
gmDrum Lowmidtom            = Z.Low_mid_tom
gmDrum Himidtom             = Z.High_mid_tom
gmDrum Crashcymbala         = Z.Crash_cymbal_1
gmDrum Crashcymbal          = id                  Z.High_wood_block
gmDrum Hightom              = Z.High_tom
gmDrum Ridecymbala          = Z.Ride_cymbal_1
gmDrum Ridecymbal           = id                  Z.Low_wood_block
gmDrum Chinesecymbal        = Z.Chinese_cymbal
gmDrum Ridebell             = Z.Ride_bell
gmDrum Splashcymbal         = Z.Splash_cymbal
gmDrum Cowbell              = Z.Cowbell
gmDrum Crashcymbalb         = Z.Crash_cymbal_2
gmDrum Vibraslap            = Z.Vibraslap
gmDrum Ridecymbalb          = Z.Ride_cymbal_2


-- Turn a Drum into a Pitch
drumPitch :: DrumsStyle -> Pitch 
drumPitch = gmDrumPitch . gmDrum

drumEvent :: Pitch -> Duration -> H.Tile
drumEvent p d = maybe (H.spacer d) (drumTile `flip` d) (drumName p)

drumName :: Pitch -> Maybe DrumsStyle
drumName = maybe Nothing fn . pitchToGmDrum where
  fn Z.Acoustic_bass_drum     = Just Acousticbassdrum
  fn Z.Bass_drum_1            = Just Bassdrum 
  fn Z.Side_stick             = Just Sidestick 
  fn Z.Acoustic_snare         = Just Acousticsnare 
  fn Z.Short_guiro            = Just Snare                  
  fn Z.Hand_clap              = Just Handclap   
  fn Z.Electric_snare         = Just Electricsnare 
  fn Z.Low_floor_tom          = Just Lowfloortom
  fn Z.Closed_hi_hat          = Just Closedhihat 
  fn Z.Long_guiro             = Just Hihat                  
  fn Z.High_floor_tom         = Just Highfloortom 
  fn Z.Pedal_hi_hat           = Just Pedalhihat 
  fn Z.Low_tom                = Just Lowtom 
  fn Z.Open_hi_hat            = Just Openhihat 
  fn Z.Claves                 = id $ Just Halfopenhihat                  
  fn Z.Low_mid_tom            = Just Lowmidtom 
  fn Z.High_mid_tom           = Just Himidtom 
  fn Z.Crash_cymbal_1         = Just Crashcymbala
  fn Z.High_wood_block        = id $ Just Crashcymbal                 
  fn Z.High_tom               = Just Hightom 
  fn Z.Ride_cymbal_1          = Just Ridecymbala 
  fn Z.Low_wood_block         = id $ Just Ridecymbal                 
  fn Z.Chinese_cymbal         = Just Chinesecymbal 
  fn Z.Ride_bell              = Just Ridebell
  fn Z.Splash_cymbal          = Just Splashcymbal
  fn Z.Cowbell                = Just Cowbell
  fn Z.Crash_cymbal_2         = Just Crashcymbalb 
  fn Z.Vibraslap              = Just Vibraslap 
  fn Z.Ride_cymbal_2          = Just Ridecymbalb          
  fn _                        = Nothing



drumTile :: DrumsStyle -> Duration -> H.Tile
drumTile Acousticbassdrum       d = H.acousticbassdrum'   d
drumTile Bassdrum               d = H.bassdrum'           d
drumTile Sidestick              d = H.sidestick'          d
drumTile Acousticsnare          d = H.acousticsnare'      d
drumTile Snare                  d = H.snare'              d
drumTile Handclap               d = H.handclap'           d
drumTile Electricsnare          d = H.electricsnare'      d
drumTile Lowfloortom            d = H.lowfloortom'        d
drumTile Closedhihat            d = H.closedhihat'        d
drumTile Hihat                  d = H.hihat'              d
drumTile Highfloortom           d = H.highfloortom'       d
drumTile Pedalhihat             d = H.pedalhihat'         d
drumTile Lowtom                 d = H.lowtom'             d
drumTile Openhihat              d = H.openhihat'          d
drumTile Halfopenhihat          d = H.halfopenhihat'      d
drumTile Lowmidtom              d = H.lowmidtom'          d
drumTile Himidtom               d = H.himidtom'           d
drumTile Crashcymbala           d = H.crashcymbala'       d
drumTile Crashcymbal            d = H.crashcymbal'        d
drumTile Hightom                d = H.hightom'            d
drumTile Ridecymbala            d = H.ridecymbala'        d
drumTile Ridecymbal             d = H.ridecymbala'        d
drumTile Chinesecymbal          d = H.chinesecymbal'      d
drumTile Ridebell               d = H.ridebell'           d
drumTile Splashcymbal           d = H.splashcymbal'       d
drumTile Cowbell                d = H.cowbell'            d
drumTile Crashcymbalb           d = H.crashcymbalb'       d
drumTile Vibraslap              d = H.vibraslap'          d
drumTile Ridecymbalb            d = H.ridecymbalb'        d

          
{-
   '((drums-style .
     (
      (acousticbassdrum () #f -3)
      (bassdrum () #f -3)
      (sidestick cross #f 1)
      (acousticsnare () #f 1)
      (snare () #f 1)
      (handclap triangle #f 1)
      (electricsnare () #f 1)
      (lowfloortom () #f -4)
      (closedhihat cross "stopped" 3)
      (hihat cross #f 3)
      (highfloortom () #f -2)
      (pedalhihat cross #f -5)
      (lowtom () #f -1)
      (openhihat cross "open" 3)
      (halfopenhihat xcircle #f 3)
      (lowmidtom () #f 0)
      (himidtom () #f 2)
      (crashcymbala xcircle #f 5)
      (crashcymbal xcircle #f 5)
      (hightom () #f 4)
      (ridecymbala cross #f 5)
      (ridecymbal cross #f 5)
      (chinesecymbal mensural #f 5)
      (ridebell () #f 5)
      (splashcymbal diamond #f 5)
      (cowbell triangle #f 5)
      (crashcymbalb cross #f 5)
      (vibraslap diamond #f 4)
      (ridecymbalb cross #f 5)
      ))
-}      

  
   