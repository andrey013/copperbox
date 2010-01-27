{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  M2.OneList
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data type for non-empty lists.
--
--------------------------------------------------------------------------------

module M2.OneList
  (
    OneList
   
  ) where


import Data.Semigroup   -- package: algebra

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

infixr 5 :+

data OneList a = One a | a :+ OneList a
  deriving (Eq)

instance Show a => Show (OneList a) where
  show = ('{':) . ($ []) . step where
     step (One a)   = shows a . showChar '}'
     step (a :+ xs) = shows a . showChar ',' . step xs


instance Functor OneList where
  fmap f (One a)        = One $ f a
  fmap f (a :+ as)      = f a :+ fmap f as

instance Foldable OneList where
  foldMap f (One a)     = f a
  foldMap f (a :+ as)   = f a `mappend` foldMap f as

instance Traversable OneList where
  traverse f (One a)    = One  <$> f a
  traverse f (a :+ as)  = (:+) <$> f a <*> traverse f as


instance Semigroup (OneList e) where
  (One x)   `append` ys  = x :+ ys
  (x :+ xs) `append` ys  = x :+ (xs `append` ys)

