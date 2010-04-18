{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Core.Interval
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pitch represention
--
--------------------------------------------------------------------------------

module Bala.Core.Interval 
  ( 
  -- * Datatypes
    Interval
  , arithmeticDistance
  , semitoneCount
  , IntervalQuality(..) 

  -- * Type classes
  , IntervalContent(..)

  -- * Operations
  , makeInterval
  , intervalQuality
  , isComplement
  , intervalName
  , intervalPair
  , addOctave
  , divSimple


  ) where

import Bala.Core.Invert
import Bala.Core.Modulo

import Data.AdditiveGroup -- VectorSpace

import Data.Monoid

--------------------------------------------------------------------------------
-- Datatypes


data Interval = Interval { arithmeticDistance :: Int, semitoneCount :: Int }
  deriving Eq


data IntervalQuality = Diminished Int | Minor | Perfect | Major | Augmented Int
  deriving (Eq)



--------------------------------------------------------------------------------
-- Type classes 

-- | Extract the pitch content from some aggregate object (e.g. a chord).
class IntervalContent c where
  intervalContent :: c -> [Interval]

  
--------------------------------------------------------------------------------
-- Instances



instance Show Interval where
  showsPrec p (Interval ad sc) = showsPrec p (ad,sc)

-- Note the @mempty@ instance is the unison interval (1,0).
instance Monoid Interval where
  mempty = Interval 1 0 
  (Interval ad sc) `mappend` (Interval ad' sc') = Interval (ad+ad'-1) (sc+sc')


instance AdditiveGroup Interval where
  zeroV = mempty
  (^+^) = mappend
  negateV (Interval ad sc) = Interval ad (negate sc)  -- !!

-- This will need some quickchecking...

instance Invert Interval where
   -- Erk - this is correct only for simple intervals...
  invert (Interval ad sc) = Interval ad' (12 - sc)
    where
      ad' = ad `rdif` 9



instance Show IntervalQuality where
  showsPrec _ (Diminished n) = showString $ replicate n 'd'
  showsPrec _ Minor          = showChar 'm'
  showsPrec _ Perfect        = showChar 'P'
  showsPrec _ Major          = showChar 'M'
  showsPrec _ (Augmented n)  = showString $ replicate n 'A'

--------------------------------------------------------------------------------

-- | @makeInterval arithmetic-distance semintone-count@ - 0 is an
-- illegal value for arithmetic distance and will generate a 
-- runtime error. 
makeInterval :: Int -> Int -> Interval
makeInterval i j | i /= 0    = Interval i j
                 | otherwise = error msg
  where
    msg = "Interval.makeInterval - cannot make interval with arthimetic " 
       ++ " distance == 0."


-- | rdif is the analogue to subtraction on arithmetic distances, 
-- but it is the difference between the larger and the smaller 
-- and hence will always generate a positive answer.
rdif :: Int -> Int -> Int
rdif a b = max a b - ((min a b) - 1)


isComplement :: Interval -> Interval -> Bool
isComplement a b = a `mappend` b == makeInterval 8 12 


intervalQuality :: Interval -> IntervalQuality
intervalQuality (Interval ad sc) = 
    either (dpa $ mod12 sc) (dmma $ mod12 sc) $ genRegular $ ad
  where
    dpa s n | s > n     = Augmented (s-n)
            | s < n     = Diminished (n-s)
            | otherwise = Perfect

    dmma s (mn,mj) | s == mn  = Minor
                   | s == mj  = Major
                   | s <  mn  = Diminished (mn-s)
                   | s >  mj  = Augmented (s-mj)
                   | otherwise = error "intervalQuality - unreachable"
                     
genRegular :: Int -> Either Int (Int,Int)
genRegular = fn . amod7 where
  fn 1 = Left   0
  fn 2 = Right (1,2)
  fn 3 = Right (3,4)
  fn 4 = Left   5 
  fn 5 = Left   7
  fn 6 = Right (8,9)
  fn 7 = Right (10,11)
  fn _ = error "genRegular - unreachable"


intervalName :: Interval -> String
intervalName ival@(Interval ad _) = show (intervalQuality ival) ++ show ad

intervalPair :: Interval -> (Int,Int)
intervalPair (Interval i j) = (i,j)

addOctave :: Interval -> Interval
addOctave = mappend (makeInterval 8 12)


-- amod7 [1-7]
amod7 :: Int -> Int
amod7 i = 1 + ((i-1) `mod` 7) 



-- Simple interval from compound interval
divSimple :: Interval -> (Int,Interval)
divSimple (Interval ad sc) = (d, Interval (amod7 ad) sc')
  where
    (d,sc') = sc `divMod` 12


