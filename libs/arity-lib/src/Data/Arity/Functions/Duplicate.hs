{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Arity.Function.Duplicate
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Arity extended combinators
-- 
-- Duplication
--
--------------------------------------------------------------------------------

module Data.Arity.Functions.Duplicate
  (
    dup
  , dup3
  , dup4
  , dup5
  , dup6
  , duplicate
  , duplicate3
  , duplicate4
  , duplicate5
  , duplicate6

  ) where


dup             :: a -> (a,a)
dup a           = (a,a)

dup3            :: a -> (a,a,a)
dup3 a          = (a,a,a)

dup4            :: a -> (a,a,a,a)
dup4 a          = (a,a,a,a)

dup5            :: a -> (a,a,a,a,a)
dup5 a          = (a,a,a,a,a)

dup6            :: a -> (a,a,a,a,a,a)
dup6 a          = (a,a,a,a,a,a)

-- Warbler
duplicate :: (a -> a -> b) -> a -> b
duplicate f x = f x x

duplicate3 :: (a -> a -> a -> b) -> a -> b
duplicate3 f x = f x x x

duplicate4 :: (a -> a -> a -> a -> b) -> a -> b
duplicate4 f x = f x x x x

duplicate5 :: (a -> a -> a -> a -> a -> b) -> a -> b
duplicate5 f x = f x x x x x

duplicate6 :: (a -> a -> a -> a -> a -> a -> b) -> a -> b
duplicate6 f x = f x x x x x x

