
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Base.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A minimal representation for Pitch and Duration.
--
--------------------------------------------------------------------------------

module Bala.Perform.Base.Datatypes ( 
  -- Internal types
  Pitch(..),
  PitchLetter(..),
  Accidental(..),
  Duration(..), 
  
  toRatio, fromDouble, toDouble,
  
  -- * Classes

  -- * lilyPond helpers
  middleC, quarternote, octaveDist,
  
  -- * Midi helpers
  midiPitch, midiTicks
  ) where

import Data.Ratio
import Data.Word
import Text.PrettyPrint.Leijen

data Pitch = Pitch {
    pitch_letter  :: PitchLetter,
    accidental    :: Accidental,
    octave        :: Int
  }
  
  
data PitchLetter = C | D | E | F | G | A | B
  deriving (Eq,Enum,Ord,Show)
  
data Accidental = Nat | Sharp | Flat | DoubleSharp | DoubleFlat
  deriving (Eq,Enum,Ord,Show)
  
-- | Duration is basically a Data.Ratio but that normalizes:
-- 6/8 becomes 3/4.
-- We might want the unnormalized value

-- TO DO - durations should probably include dotting!
newtype Duration = Duration { unDuration :: (Int,Int) }
  deriving (Eq,Ord)
  
toRatio (Duration (n,d)) = (fromIntegral n) % (fromIntegral d)

toDouble :: Duration -> Double
toDouble (Duration (n,d)) = (fromIntegral n) / (fromIntegral d)

fromDouble :: Double -> Duration
fromDouble j = let r = toRational j; (n,d) = (numerator r, denominator r)  
               in Duration (fromIntegral n, fromIntegral d)


{-
instance Eq Duration where
  a == b = toRatio a == toRatio b
  
instance Ord  
-}





-- LilPond Helpers
middleC :: Pitch 
middleC = Pitch C Nat 4

quarternote :: Duration
quarternote = Duration (1,4)

octaveDist :: Pitch -> Pitch -> Int
octaveDist _ _ = 1


-- Helpers for Midi

midiPitch :: Pitch -> Word8
midiPitch _   = 60

midiTicks             :: Duration -> Integer
midiTicks _   = 384

--------------------------------------------------------------------------------
-- Pretty instances


instance Pretty Pitch where
  pretty (Pitch l a o)  = group $ pretty l <> pretty a <> int o
  
  
instance Pretty PitchLetter where 
  pretty              = text . show
  
instance Pretty Accidental where
  pretty Nat          = empty
  pretty Sharp        = char '#'
  pretty Flat         = char 'b'
  pretty DoubleSharp  = text "##" 
  pretty DoubleFlat   = text "bb"

  

instance Pretty Duration where
  pretty (Duration (n,d)) = group $ int n <> char '/' <> int d
  
  
  
      