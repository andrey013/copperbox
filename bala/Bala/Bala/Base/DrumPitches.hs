
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

drumEvent :: Pitch -> Duration -> Either Duration (H.Mark H.DrumMark,Duration) 
drumEvent p d = maybe (Left d) (\a -> Right (drumMark a,d)) (drumName p)

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



drumMark :: DrumsStyle -> H.Mark H.DrumMark
drumMark Acousticbassdrum       = H.acousticbassdrum'   
drumMark Bassdrum               = H.bassdrum'           
drumMark Sidestick              = H.sidestick'          
drumMark Acousticsnare          = H.acousticsnare'      
drumMark Snare                  = H.snare'              
drumMark Handclap               = H.handclap'           
drumMark Electricsnare          = H.electricsnare'      
drumMark Lowfloortom            = H.lowfloortom'        
drumMark Closedhihat            = H.closedhihat'        
drumMark Hihat                  = H.hihat'              
drumMark Highfloortom           = H.highfloortom'       
drumMark Pedalhihat             = H.pedalhihat'         
drumMark Lowtom                 = H.lowtom'             
drumMark Openhihat              = H.openhihat'          
drumMark Halfopenhihat          = H.halfopenhihat'      
drumMark Lowmidtom              = H.lowmidtom'          
drumMark Himidtom               = H.himidtom'           
drumMark Crashcymbala           = H.crashcymbala'       
drumMark Crashcymbal            = H.crashcymbal'        
drumMark Hightom                = H.hightom'            
drumMark Ridecymbala            = H.ridecymbala'        
drumMark Ridecymbal             = H.ridecymbala'        
drumMark Chinesecymbal          = H.chinesecymbal'      
drumMark Ridebell               = H.ridebell'           
drumMark Splashcymbal           = H.splashcymbal'       
drumMark Cowbell                = H.cowbell'            
drumMark Crashcymbalb           = H.crashcymbalb'       
drumMark Vibraslap              = H.vibraslap'          
drumMark Ridecymbalb            = H.ridecymbalb'        
          
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

  
   