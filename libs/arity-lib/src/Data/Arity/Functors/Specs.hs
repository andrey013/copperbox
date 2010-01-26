{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Arity.Functors.Specs
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Arity extended combinators
-- 
-- Functor composition
--
--------------------------------------------------------------------------------

module Data.Arity.Functors.Specs
  (
    oo
  , ooo
  , oooo
  , ooooo

  ) where



-- | Compose an arity 1 functor with an arity 2 functor.
--
oo        :: (Functor f, Functor g) 
          => (a -> b) -> f (g a) -> f (g b)
oo f fa = fmap (fmap f) fa


ooo       :: (Functor f, Functor g, Functor h) 
          => (a -> b) ->  f (g (h a)) -> f (g (h b))
ooo f fa = fmap (fmap (fmap f)) fa



oooo      :: (Functor f, Functor g, Functor h, Functor i) 
          => (a -> b) ->  f (g (h (i a))) -> f (g (h (i b)))
oooo f fa = fmap (fmap (fmap (fmap f))) fa


ooooo     :: (Functor f, Functor g, Functor h, Functor i, Functor j) 
          => (a -> b) ->  f (g (h (i (j a)))) -> f (g (h (i (j b))))
ooooo f fa = fmap (fmap (fmap (fmap (fmap f)))) fa
