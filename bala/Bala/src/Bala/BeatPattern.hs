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
    Multiplier
  , MetricalPattern(..)
  , Beat(..)
  , BeatPattern
  
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
  , leftInterp
  , interp
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


data MetricalPattern a = MetricalPattern { 
      barlen  :: Integer, 
      jlbeats :: JoinList (Beat Multiplier a) 
    }


-- | Beats represented as (N)ote beats or (R)est beats.
data Beat duration a = Nb duration a 
                     | Rb duration
  deriving (Eq,Show)

type BeatPattern = MetricalPattern ()   


--------------------------------------------------------------------------------

-- | Interpret a rest (i.e. the Rb contructor of Beat) during 
-- @zipInterp@.
class InterpretRest e where
  interpretRest :: Duration -> e


instance Functor (Beat duration) where
  fmap f (Nb d a) = Nb d (f a)
  fmap _ (Rb d)   = Rb d


durmap :: (d -> d') -> Beat d a -> Beat d' a
durmap f (Nb d a) = Nb (f d) a
durmap f (Rb d)   = Rb (f d)


--------------------------------------------------------------------------------


rest :: Integer -> MetricalPattern a
rest i = MetricalPattern i (wrap $ Rb i)


beat :: Integer -> BeatPattern
beat i = MetricalPattern i (wrap $ Nb i ())


beats :: [Integer] -> BeatPattern
beats xs = MetricalPattern (sum xs) (fromList $ map (Nb `flip` ()) xs)

infixr 2 ><
(><) :: MetricalPattern a -> MetricalPattern a -> MetricalPattern a
a >< b = MetricalPattern (barlen a + barlen b) (jlbeats a `mappend` jlbeats b)


infixr 1 //
(//) :: MetricalPattern a -> MetricalPattern a -> MetricalPattern a
a // b | barlen a == barlen b = MetricalPattern (barlen b) (jlbeats a `mappend` jlbeats b)
       | otherwise            = error $ "unmatched bars"

-- | Repeat a beat pattern n times.
times :: Int -> MetricalPattern a -> MetricalPattern a
times n (MetricalPattern len app) = MetricalPattern len (JL.repeated n app)


--------------------------------------------------------------------------------
-- Evaluation

zipInterp :: InterpretRest e 
          => [Duration -> a -> e] -> [Beat Rational a] -> [e]
zipInterp fs     (Rb n:ys)   = interpretRest n : zipInterp fs ys
zipInterp (f:fs) (Nb n a:ys) = f n a : zipInterp fs ys
zipInterp _      _           = []

-- Ho hum ... too many interp functions...
interp :: InterpretRest e 
          => (Duration -> a -> e) -> [Beat Rational a] -> [e]
interp f (Rb n:ys)   = interpretRest n : interp f ys
interp f (Nb n a:ys) = f n a : interp f ys
interp _      _      = []


leftInterp :: InterpretRest e 
           => [Duration -> e] -> [Beat Rational a] -> [e]
leftInterp fs     (Rb n:ys)   = interpretRest n : leftInterp fs ys
leftInterp (f:fs) (Nb n _:ys) = f n : leftInterp fs ys
leftInterp _      _           = []


run0 :: MetricalPattern a -> [Beat Integer a]
run0 = toList . jlbeats

run1 :: Rational -> MetricalPattern a -> [Beat Rational a]
run1 t (MetricalPattern n kl) = map (durmap ((t*) . (%n))) $ toList kl


unitBeat :: MetricalPattern a -> MetricalPattern a
unitBeat (MetricalPattern i kl) = MetricalPattern i $ expand kl
  where
    expand = gfold fE fS fJ
    fE        = JL.empty
    fS (Nb n a) | n <= 1    = wrap $ Nb n a
                | otherwise = mappend (wrap $ Nb 1 a) 
                                    (JL.replicate (fromIntegral $ n-1) (Rb 1))
    fS (Rb n) | n <= 1    = wrap $ Rb n
              | otherwise = JL.replicate (fromIntegral n) (Rb 1)
    fJ        = mappend