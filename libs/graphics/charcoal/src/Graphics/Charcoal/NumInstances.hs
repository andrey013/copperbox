{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Charcoal.NumInstances
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Acknowledgment - these are the /folklore/ instances of Num etc.
-- for functions.
--
-- Note - there are prettier definitions in Conal Elliott's 
-- /Beautiful Differentiation/ for example.
--
--------------------------------------------------------------------------------


module Graphics.Charcoal.NumInstances where 


instance Show (a -> b) where
  show _ = "_FUNCTION_"

instance Eq (a -> b) where
  (==) _ _ = error "No (==) on functions after all..."

instance Num b => Num (a -> b) where
  f + g = \a -> f a + g a
  f - g = \a -> f a - g a
  f * g = \a -> f a * g a
  abs    f = \a -> abs $ f a
  signum f = \a -> signum $ f a
  fromInteger = const . fromInteger
  
instance Fractional b => Fractional (a -> b) where
  fromRational = const . fromRational
  recip f = \a -> recip $ f a


instance Floating b => Floating (a -> b) where
  pi     = const pi
  sqrt f = \a -> sqrt $ f a 
  exp  f = \a -> exp  $ f a
  log  f = \a -> log  $ f a
  f ** g = \a -> f a ** g a

  sin   f = \a -> sin   $ f a
  cos   f = \a -> cos   $ f a
  asin  f = \a -> asin  $ f a
  acos  f = \a -> acos  $ f a
  atan  f = \a -> atan  $ f a
  sinh  f = \a -> sinh  $ f a
  cosh  f = \a -> cosh  $ f a
  asinh f = \a -> asinh $ f a
  acosh f = \a -> acosh $ f a
  atanh f = \a -> atanh $ f a
  f `logBase` g = \a -> f a `logBase` g a
