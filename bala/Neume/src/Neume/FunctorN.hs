{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.FunctorN
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bi-functors and tenary functors.
--
--------------------------------------------------------------------------------


module Neume.FunctorN
  ( 
  -- * Stateful map
    FMap2(..)
  , FMap3(..)
  , fmap2a
  , fmap3a
  , fmap3b

  ) where 



class FMap2 f where
  fmap2 :: (a -> u) -> (b -> v) -> f a b -> f u v

class FMap3 f where
  fmap3 :: (a -> u) -> (b -> v) -> (c -> w) -> f a b c -> f u v w


fmap2a :: FMap2 f => (a -> u) -> f a b -> f u b
fmap2a f = fmap2 f id 

fmap3a :: FMap3 f => (a -> u) -> f a b c -> f u b c
fmap3a f = fmap3 f id id
   
fmap3b :: FMap3 f => (b -> v) -> f a b c -> f a v c
fmap3b g = fmap3 id g id

instance FMap2 (,) where
  fmap2 f g (a,b) = (f a, g b)

instance FMap2 Either where
  fmap2 f _ (Left a)  = Left (f a)
  fmap2 _ g (Right b) = Right (g b)

