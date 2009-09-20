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
  , unitBeat

  ) where

import Bala.Duration
import Bala.Kleene


import Data.Ratio

--------------------------------------------------------------------------------
-- Data types

type Multiplier = Integer

-- | Beats represented as (N)ote beats or (R)est beats.
data Beat a = Nb a | Rb a
  deriving (Eq,Show)

data BeatPattern = BeatPattern { 
      barlen :: Integer, 
      kleene :: Kleene (Beat Multiplier) 
    }

--------------------------------------------------------------------------------

-- | Interpret a rest (i.e. the Rb contructor of Beat) during 
-- @zipInterp@.
class InterpretRest e where
  interpretRest :: Duration -> e


instance Functor Beat where
  fmap f (Nb a) = Nb (f a)
  fmap f (Rb a) = Rb (f a)

--------------------------------------------------------------------------------


rest :: Integer -> BeatPattern
rest i = BeatPattern i (O $ Rb i)


beat :: Integer -> BeatPattern
beat i = BeatPattern i (O $ Nb i)


beats :: [Integer] -> BeatPattern
beats xs = BeatPattern (sum xs) (fromList $ map Nb xs)

-- This one might be more confusing 
-- anacrusis :: Integer -> BeatPatten
-- anacrusis i = BeatPattern i id

infixr 2 ><
(><) :: BeatPattern -> BeatPattern -> BeatPattern
a >< b = BeatPattern (barlen a + barlen b) (S (kleene a) (kleene b))


infixr 1 //
(//) :: BeatPattern -> BeatPattern -> BeatPattern
a // b | barlen a == barlen b = BeatPattern (barlen b) (S (kleene a) (kleene b))
       | otherwise            = error $ "unmatched bars"

-- | Repeat a beat pattern n times.
times :: Int -> BeatPattern -> BeatPattern
times n (BeatPattern len app) = BeatPattern len (R n app)


--------------------------------------------------------------------------------
-- Evaluation

zipInterp :: InterpretRest e => [Duration -> e] -> [Beat Rational] -> [e]
zipInterp fs     (Rb n:ys) = interpretRest n : zipInterp fs ys
zipInterp (f:fs) (Nb n:ys) = f n : zipInterp fs ys
zipInterp _      _        = []


run0 :: BeatPattern -> [Beat Integer]
run0 = toList . kleene

run1 :: Rational -> BeatPattern -> [Beat Rational]
run1 t (BeatPattern n kl) = map (fmap (\i -> t * (i%n))) $ toList kl


unitBeat :: BeatPattern -> BeatPattern
unitBeat (BeatPattern i kl) = BeatPattern i $ expand kl
  where
    expand :: Kleene (Beat Multiplier) -> Kleene (Beat Multiplier)
    expand = gfold fE fO fS fR fA
    fE   = E
    fO (Nb n) | n <= 1    = O $ Nb n
              | otherwise = S (O $ Nb 1) (R (fromIntegral $ n-1) (O $ Rb 1))
    fO (Rb n) | n <= 1    = O $ Rb n
              | otherwise = R (fromIntegral n) (O $ Rb 1)
    fS   = S
    fR   = R
    fA   = A  
