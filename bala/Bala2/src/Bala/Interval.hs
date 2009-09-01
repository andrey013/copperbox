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
import Bala.RetroInt

import Data.AdditiveGroup -- VectorSpace

import Data.Monoid
import Test.QuickCheck

--------------------------------------------------------------------------------
-- Datatypes


data Interval = Interval { arithmeticDistance :: RI, semitoneCount :: Int }
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
  arbitrary = arbitrary >>= \i -> geither i (genRegular $ fromRI i)
    where
      geither i (Left j)      = return $ Interval i j
      geither i (Right (j,k)) = elements [j,k] >>= \l -> return (Interval i l)
  coarbitrary (Interval ad sc) = coarbitrary ad . coarbitrary sc

instance Monoid Interval where
  mempty = Interval mempty 0 
  (Interval ad sc) `mappend` (Interval ad' sc') = Interval (ad `mappend` ad') (sc+sc')


instance AdditiveGroup Interval where
  zeroV = mempty
  (^+^) = mappend
  negateV (Interval ad sc) = Interval ad (negate sc)  -- !!

-- This will need some quickchecking...

instance Invert Interval where
   -- Erk - this is correct only for simple intervals...
  invert (Interval ad sc) = Interval ad' (12 - sc)
    where
      ad' = ad `rdif` toRI (9::Int)


instance Show IntervalQuality where
  showsPrec _ (Diminished n) = showString $ replicate n 'd'
  showsPrec _ Minor          = showChar 'm'
  showsPrec _ Perfect        = showChar 'P'
  showsPrec _ Major          = showChar 'M'
  showsPrec _ (Augmented n)  = showString $ replicate n 'A'

--------------------------------------------------------------------------------

complement :: Interval -> Interval -> Bool
complement a b = a `mappend` b == makeInterval 8 12 


intervalQuality :: Interval -> IntervalQuality
intervalQuality (Interval ad sc) = 
    either (dpa $ mod12 sc) (dmma $ mod12 sc) $ genRegular $ fromRI ad
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

makeInterval :: Int -> Int -> Interval
makeInterval i j = Interval (toRI i) j

intervalPair :: Interval -> (Int,Int)
intervalPair (Interval i j) = (fromRI i,j)


-- 2 3 6 7
major2 :: Interval
major3 :: Interval
major6 :: Interval
major7 :: Interval
major2 = makeInterval 2 2
major3 = makeInterval 3 4
major6 = makeInterval 6 9
major7 = makeInterval 7 11


minor2 :: Interval
minor3 :: Interval
minor6 :: Interval
minor7 :: Interval
minor2 = makeInterval 2 1
minor3 = makeInterval 3 3
minor6 = makeInterval 6 8
minor7 = makeInterval 7 10


perfect1 :: Interval 
perfect4 :: Interval 
perfect5 :: Interval 
perfect8 :: Interval 
perfect1 = makeInterval 1 0
perfect4 = makeInterval 4 5
perfect5 = makeInterval 5 7
perfect8 = makeInterval 8 12


-- amod7 [1-7]
amod7 :: Int -> Int
amod7 i = 1 + ((i-1) `mod` 7) 

amod7' :: RI -> RI
amod7' i = toRI $  1 + (((fromRI i)-1) `mod` 7) 



-- Simple interval from compound interval
divSimple :: Interval -> (Int,Interval)
divSimple (Interval ad sc) = (d, Interval (amod7' ad) sc')
  where
    (d,sc') = sc `divMod` 12
