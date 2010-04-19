{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Core.Tonal
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

module Bala.Core.Tonal where

import Bala.Core.Utils ( rap, iter )

import Data.AdditiveGroup
import Data.AffineSpace



data PitchLetter = C | D | E | F | G | A | B
  deriving (Bounded,Enum,Eq,Ord,Show)


-- Note - countup does not address the /double counting/ of
-- arithmetic distance
--
countup :: PitchLetter -> Int -> PitchLetter
countup p i = fromEnum p `rap` (+i) `rap` (`mod` 7) `rap` toEnum 

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

--------------------------------------------------------------------------------
-- Datatypes


data Pitch = Pitch 
      { pitch_letter      :: PitchLetter
      , pitch_semitones   :: Int
      }
  deriving Eq


middle_c    :: Pitch 
middle_c    = Pitch C 60


type Accidental = Int   -- 0 Nat, negative Flat, positive Sharp

type Octave = Int

standardForm :: Pitch -> (PitchLetter,Accidental,Octave)
standardForm (Pitch l sc) = (l,acctl,oct)
  where
    (o1,carry)  = sc `divMod` 12
    a1          = carry - plSemitones l
    (oct,acctl) = bump o1 a1
    
    -- experimentally this works, but what does it mean as an
    -- algorithm? 
    bump o a    | a < -5    = (o-1,12-(abs a))
                | otherwise = (o,a)


instance Show Pitch where
  showsPrec _ pch = shows pl . showsAcctl a . showChar '.' . shows oct
    where
      (pl,a,oct) = standardForm pch
      showsAcctl i | i < 0     = iter (abs i) (showChar 'b')
                   | i > 0     = iter i       (showChar '#')
                   | otherwise = showString "nat"

                   


data Interval = Interval 
       { arithmetic_distance  :: Int
       , semitone_count       :: Int 
       }
  deriving (Eq,Ord)


instance AdditiveGroup Interval where
  zeroV  = Interval 0 0
  (Interval ad sc) ^+^ (Interval ad' sc') = Interval (ad + ad') (sc + sc')
  negateV (Interval ad sc) = Interval (negate ad) (negate sc)  



instance AffineSpace Pitch where
  type Diff Pitch = Interval

  (Pitch l sc) .-. (Pitch l' sc') = Interval (l .-. l') (sc - sc')

  (Pitch l sc) .+^ (Interval ad isc) = Pitch (l .+^ ad) (sc + isc)


