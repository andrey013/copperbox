
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Base.Datatypes
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

module HNotate.Base.Datatypes (
    -- Internal types
    Pitch(..),
    PitchLetter(..),
    Accidental(..),
    Duration(..),

    toRatio, fromDouble, toDouble,

    -- * Operations

    semitones,
    arithmeticDistance,
    
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
  deriving (Eq)

data PitchLetter = C | D | E | F | G | A | B
  deriving (Eq,Enum,Ord,Show)

data Accidental = DoubleFlat | Flat | Nat | Sharp  | DoubleSharp 
  deriving (Eq,Enum,Ord,Show)

-- | Duration is basically Data.Ratio but that normalizes:
-- 6/8 becomes 3/4.
-- We might want the unnormalized value

-- TO DO - durations should probably include dotting!
newtype Duration = Duration { getDuration :: (Int,Int) }
  deriving (Eq,Ord)

toRatio (Duration (n,d)) = (fromIntegral n) % (fromIntegral d)

toDouble :: Duration -> Double
toDouble (Duration (n,d)) = (fromIntegral n) / (fromIntegral d)

fromDouble :: Double -> Duration
fromDouble j = let r = toRational j; (n,d) = (numerator r, denominator r)
               in Duration (fromIntegral n, fromIntegral d)

instance Ord Pitch where
  compare p1 p2 = semitones p1 `compare` semitones p2


semitones :: Pitch -> Int
semitones (Pitch l a o) = semis l + asemis a + (12 * o)

semis C = 0
semis D = 2
semis E = 4
semis F = 5
semis G = 7
semis A = 9
semis B = 11

asemis Nat          = 0
asemis Sharp        = 1
asemis Flat         = (-1)
asemis DoubleSharp  = 2
asemis DoubleFlat   = (-2)

-- LilPond Helpers
middleC :: Pitch
middleC = Pitch C Nat 4

quarternote :: Duration
quarternote = Duration (1,4)


-- See Lilypond (6.1.6 - relative octaves)
-- ceses ->- fisis
-- cbb   ->- f##   -- fourth 
octaveDist :: Pitch -> Pitch -> Int
octaveDist p p' = 
    fn (abs $ arithmeticDistance p p') (if p > p' then negate else id) 
  where
    fn dist f 
      | dist <= 4 = 0
      | otherwise = f $ (dist - 4) `div` 6 + 1 
   
arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance (Pitch l _ o) (Pitch l' _ o') = 
    dist (o * 7 + fromEnum l) (o' * 7 + fromEnum l')
  where
    dist i i'
      | i > i'      = negate $ 1 + (i - i')
      | otherwise   = 1 + (i' - i)


-- Helpers for Midi

    
    
midiPitch :: Pitch -> Word8
midiPitch (Pitch l a o) = fromIntegral $ semis l + acci a + (12 * (o+1)) 
  where

    
    acci = (\x -> x - 2) . fromEnum 



midiTicks :: Integer -> Duration -> Integer
midiTicks tpqn d = floor $ fromIntegral (4 * tpqn) * toDouble d

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



