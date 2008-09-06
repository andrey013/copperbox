
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.MusicRepDatatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Datatypes for representing music - principally these are the islands in
-- the LilyPond or Abc input files that control the note list generation
-- (e.g. key signature, meter)
--
--------------------------------------------------------------------------------

module HNotate.MusicRepDatatypes where

import HNotate.Duration
import HNotate.Pitch

 
                      
--------------------------------------------------------------------------------
-- Music representation

data Key = Key Pitch Mode
  deriving Show  

data Meter = TimeSig Int Int | CommonTime | CutTime
  deriving Show
  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving Show


c_major = Key middle_c Major  

four_four = TimeSig 4 4

meterToDouble :: Meter -> Double
meterToDouble (TimeSig n d) = (fromIntegral n) / (fromIntegral d)
meterToDouble CommonTime    = 4.0 / 4.0
meterToDouble CutTime       = 2.0 / 2.0



  
