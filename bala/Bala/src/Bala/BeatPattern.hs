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

import Data.JoinList ( JoinList, wrap, gfold, fromList, toList )
import qualified Data.JoinList as JL

import Data.Monoid
import Data.Ratio

--------------------------------------------------------------------------------
-- Data types

type Multiplier = Integer

-- | Beats represented as (N)ote beats or (R)est beats.
data Beat a = Nb a | Rb a
  deriving (Eq,Show)

data BeatPattern = BeatPattern { 
      barlen  :: Integer, 
      jlbeats :: JoinList (Beat Multiplier) 
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
rest i = BeatPattern i (wrap $ Rb i)


beat :: Integer -> BeatPattern
beat i = BeatPattern i (wrap $ Nb i)


beats :: [Integer] -> BeatPattern
beats xs = BeatPattern (sum xs) (fromList $ map Nb xs)

-- This one might be more confusing 
-- anacrusis :: Integer -> BeatPatten
-- anacrusis i = BeatPattern i id

infixr 2 ><
(><) :: BeatPattern -> BeatPattern -> BeatPattern
a >< b = BeatPattern (barlen a + barlen b) (jlbeats a `mappend` jlbeats b)


infixr 1 //
(//) :: BeatPattern -> BeatPattern -> BeatPattern
a // b | barlen a == barlen b = BeatPattern (barlen b) (jlbeats a `mappend` jlbeats b)
       | otherwise            = error $ "unmatched bars"

-- | Repeat a beat pattern n times.
times :: Int -> BeatPattern -> BeatPattern
times n (BeatPattern len app) = BeatPattern len (JL.repeated n app)


--------------------------------------------------------------------------------
-- Evaluation

zipInterp :: InterpretRest e => [Duration -> e] -> [Beat Rational] -> [e]
zipInterp fs     (Rb n:ys) = interpretRest n : zipInterp fs ys
zipInterp (f:fs) (Nb n:ys) = f n : zipInterp fs ys
zipInterp _      _        = []


run0 :: BeatPattern -> [Beat Integer]
run0 = toList . jlbeats

run1 :: Rational -> BeatPattern -> [Beat Rational]
run1 t (BeatPattern n kl) = map (fmap (\i -> t * (i%n))) $ toList kl


unitBeat :: BeatPattern -> BeatPattern
unitBeat (BeatPattern i kl) = BeatPattern i $ expand kl
  where
    expand :: JoinList (Beat Multiplier) -> JoinList (Beat Multiplier)
    expand = gfold fE fS fJ
    fE        = JL.empty
    fS (Nb n) | n <= 1    = wrap $ Nb n
              | otherwise = mappend (wrap $ Nb 1) 
                                    (JL.replicate (fromIntegral $ n-1) (Rb 1))
    fS (Rb n) | n <= 1    = wrap $ Rb n
              | otherwise = JL.replicate (fromIntegral n) (Rb 1)
    fJ        = mappend