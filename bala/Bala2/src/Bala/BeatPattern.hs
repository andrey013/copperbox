{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.BeatPattern
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Beat patterns
--
--------------------------------------------------------------------------------

module Bala.BeatPattern
  ( 
  -- * Data types
    BeatPattern
  , Beat(..)
  
  -- * Type classes
  , InterpretRest(..)

  -- * Combinators
  , rest
  , beat
  , beats 
  , (><)
  , (//)
  , times 

  -- * Evaluation
  , zipInterp
  , run0
  , run1

  ) where

import Bala.Duration

import Data.Ratio

--------------------------------------------------------------------------------
-- Data types

type Multiplier = Integer

data Beat a = B a | R a
  deriving (Eq,Show)

type HBeats a = [Beat a] -> [Beat a]

data BeatPattern = BeatPattern { barlen :: Integer, apply :: HBeats Multiplier }

--------------------------------------------------------------------------------

-- | Interpret a rest (i.e. the R contructor of Beat) during 
-- @zipInterp@.
class InterpretRest e where
  interpretRest :: Duration -> e


instance Functor Beat where
  fmap f (B a) = B (f a)
  fmap f (R a) = R (f a)

--------------------------------------------------------------------------------


rep :: [Beat a] -> HBeats a
rep xs = (xs ++)

rest :: Integer -> BeatPattern
rest i = BeatPattern i (\xs -> R i:xs)


beat :: Integer -> BeatPattern
beat i = BeatPattern i (\xs -> B i:xs)


beats :: [Integer] -> BeatPattern
beats xs = BeatPattern (sum xs) (rep $ map B xs)

-- This one might be more confusing 
-- anacrusis :: Integer -> BeatPatten
-- anacrusis i = BeatPattern i id

infixr 2 ><
(><) :: BeatPattern -> BeatPattern -> BeatPattern
f >< g = BeatPattern (barlen f + barlen g) (apply f . apply g)


infixr 1 //
(//) :: BeatPattern -> BeatPattern -> BeatPattern
f // g | barlen f == barlen g = BeatPattern (barlen g) (apply f . apply g)
       | otherwise            = error $ "unmatched bars"


times :: Int -> BeatPattern -> BeatPattern
times n (BeatPattern len app) = BeatPattern len (iter n app) where
  iter i f | i <= 0     = id
           | i == 1     = f
           | otherwise  = f . iter (i-1) f
  

zipInterp :: InterpretRest e => [Duration -> e] -> [Beat Rational] -> [e]
zipInterp fs     (R n:ys) = interpretRest n : zipInterp fs ys
zipInterp (f:fs) (B n:ys) = f n : zipInterp fs ys
zipInterp _      _        = []


run0 :: BeatPattern -> [Beat Integer]
run0 = ($ []) . apply

run1 :: Rational -> BeatPattern -> [Beat Rational]
run1 t (BeatPattern n f) = map (fmap (\i -> t * (i%n))) $ f []

