{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Interval
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pitch represention
--
--------------------------------------------------------------------------------

module Bala.Interval  where

import Bala.Invert
import Bala.Modulo

import Data.AdditiveGroup

import Test.QuickCheck

--------------------------------------------------------------------------------
-- Datatypes


data Interval = Interval { arithmeticDistance :: Int, semitoneCount :: Int }
  deriving Eq


data IntervalQuality = Diminished Int | Minor | Perfect | Major | Augmented Int
  deriving (Eq)


--------------------------------------------------------------------------------
-- Type classes 


  
--------------------------------------------------------------------------------
-- Instances



instance Show Interval where
  showsPrec p (Interval ad sc) = showsPrec p (ad,sc)

instance Arbitrary Interval where
  arbitrary = arbitrary >>= \i -> geither i (genRegular i)
    where
      geither i (Left j)      = return $ Interval i j
      geither i (Right (j,k)) = elements [j,k] >>= \l -> return (Interval i l)
  coarbitrary (Interval ad sc) = coarbitrary ad . coarbitrary sc

instance AdditiveGroup Interval where
  zeroV = Interval 1 0 
  (Interval ad sc) ^+^ (Interval ad' sc') = Interval (ad + f ad') (sc+sc')
    where f i | i > 0     = i-1
              | i < 0     = i+1
              | otherwise = error "^+^ ill-formed interval, ad should not be 0."

  negateV (Interval ad sc) = Interval (-ad) (-sc)

-- This will need some quickchecking...

instance Invert Interval where
   -- Erk - this is correct only for simple intervals...
  invert (Interval ad sc) = Interval (9 - ad) (12 - sc)


instance Show IntervalQuality where
  showsPrec _ (Diminished n) = showString $ replicate n 'd'
  showsPrec _ Minor          = showChar 'm'
  showsPrec _ Perfect        = showChar 'P'
  showsPrec _ Major          = showChar 'M'
  showsPrec _ (Augmented n)  = showString $ replicate n 'A'

--------------------------------------------------------------------------------

intervalQuality :: Interval -> IntervalQuality
intervalQuality (Interval ad sc) = 
    either (dpa $ mod12 sc) (dmma $ mod12 sc) $ genRegular ad
  where
    dpa s n | s > n     = Augmented (s-n)
            | s < n     = Diminished (n-s)
            | otherwise = Perfect

    dmma s (mn,mj) | s == mn  = Minor
                   | s == mj  = Major
                   | s <  mn  = Diminished (mn-s)
                   | s >  mj  = Augmented (s-mj)
                   | otherwise = error "intervalQuality - unreachable (1)"
                     
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

-- 2 3 6 7
minor :: Int -> Interval
minor 2 = Interval 2 1
minor 3 = Interval 3 3
minor 6 = Interval 6 8
minor 7 = Interval 7 10
minor i = error $ "minor on " ++ show i

major :: Int -> Interval
major 2 = Interval 2 2
major 3 = Interval 3 4
major 6 = Interval 6 9
major 7 = Interval 7 11
major i = error $ "major on " ++ show i


-- amod7 [1-7]
amod7 :: Int -> Int
amod7 i = 1 + ((i-1) `mod` 7) 


-- Simple interval from compound interval
divSimple :: Interval -> (Int,Interval)
divSimple (Interval ad sc) = (d, Interval (amod7 ad) sc')
  where
    (d,sc') = sc `divMod` 12
