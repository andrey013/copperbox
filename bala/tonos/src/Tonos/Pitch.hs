{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Tonos.Pitch
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Alternative representation of Pitch, Intervals etc. that 
-- (hopefully) has better defined mathematical operations.
--
--------------------------------------------------------------------------------

module Tonos.Pitch 
  (
    PitchLetter(..)
  , plSemitones

  , Pitch(..)
  , middle_c

  , StandardForm
  , Accidental
  , Octave
  , standardForm

  , Interval(..)

  ) where

import Tonos.Base
import Tonos.Utils

import Data.AdditiveGroup
import Data.AffineSpace   hiding ( distance )


--------------------------------------------------------------------------------
-- Pitch letter

data PitchLetter = C | D | E | F | G | A | B
  deriving (Bounded,Enum,Eq,Ord,Show)



liftU_pl :: (Int -> Int) -> PitchLetter -> PitchLetter
liftU_pl op = toFro (\x -> (op x) `mod` 7)

liftB_pl :: (Int -> Int -> Int) -> PitchLetter -> PitchLetter -> PitchLetter
liftB_pl op a b = toEnum $ ((fromEnum a) `op` (fromEnum b)) `mod` 7
   

instance Num PitchLetter where
  (+) = liftB_pl (+)
  (-) = liftB_pl (-)
  (*) = liftB_pl (*)
  negate        = liftU_pl negate
  fromInteger i = toEnum $ (fromInteger i) `mod` 7
  signum _      = error "PitchLetters are not signed"
  abs _         = error "PitchLetters are not signed"
  

-- Note - countup does not address the /double counting/ of
-- arithmetic distance
--
countup :: PitchLetter -> Int -> PitchLetter
countup p i = p + fromIntegral i 


instance AffineSpace PitchLetter where
  type Diff PitchLetter = Int

  -- use bump for /double counting/.
  p1 .-. p2 = bumpCount $ (fromEnum p1 - fromEnum p2) `mod` 7

  p .+^ i | i > 1     = countup p (i-1) 
          | i < (-1)  = countup p (i+1)
          | otherwise = p

bumpCount :: Int -> Int
bumpCount n | n < 0     = n-1 
            | otherwise = n+1  



plSemitones :: PitchLetter -> Int
plSemitones C = 0
plSemitones D = 2
plSemitones E = 4
plSemitones F = 5
plSemitones G = 7
plSemitones A = 9
plSemitones B = 11

-- distance a b >= 0
-- distance a b == distance b a
--
instance Distance PitchLetter where
  distance a b = min ((sa-sb) `mod` 12) ((sb-sa) `mod` 12)
    where
      sa = plSemitones a; sb = plSemitones b

--------------------------------------------------------------------------------
-- Pitch


data Pitch = Pitch 
      { pitch_letter      :: PitchLetter
      , pitch_semitones   :: Int
      }
  deriving Eq


middle_c    :: Pitch 
middle_c    = Pitch C 60

type StandardForm = (PitchLetter, Accidental, Octave)

type Accidental = Int   -- 0 Nat, negative Flat, positive Sharp

type Octave = Int

-- Note 'standardForm' does not change the pitch letter, so the
-- result may have double flats, triple sharps, or more.
-- 
standardForm :: Pitch -> StandardForm
standardForm (Pitch l sc) = rebalance (l,acc,ove)
  where
    -- If you "subtract" the pitch letter then (`divMod` 12) 
    -- gives the octave and the accidental...
    --
    (ove,a0)   = (sc - plSemitones l) `divMod` 12
    
    -- But the accidental counts upwards with sharps [0..11], it 
    -- needs shifting [-5..6] for flats...
    --
    acc        = a0 `smod` 12
    
    -- And the octave needs rebalancing for flats...
    --
    rebalance (lbl,a,o) | a < 0     = (lbl,a,o+1)
                        | otherwise = (lbl,a,o)


instance Show Pitch where
  showsPrec _ pch = shows pl . showsAcctl a . showChar '.' . shows oct
    where
      (pl,a,oct) = standardForm pch
      showsAcctl i | i < 0     = showString $ replicate (abs i) 'b'
                   | i > 0     = showString $ replicate i '#'
                   | otherwise = showString "nat"

                   

--------------------------------------------------------------------------------
-- Interval

data Interval = Interval 
       { arithmetic_distance  :: Int
       , semitone_count       :: Int 
       }
  deriving (Eq,Ord)


instance Show Interval where
  showsPrec p (Interval ad sc) = showsPrec p (ad,sc)


instance AdditiveGroup Interval where
  zeroV  = Interval 0 0
  (Interval ad sc) ^+^ (Interval ad' sc') = Interval (ad + ad') (sc + sc')
  negateV (Interval ad sc) = Interval (negate ad) (negate sc)  



instance AffineSpace Pitch where
  type Diff Pitch = Interval

  (Pitch l sc) .-. (Pitch l' sc') = Interval (l .-. l') (sc - sc')

  (Pitch l sc) .+^ (Interval ad isc) = Pitch (l .+^ ad) (sc + isc)


