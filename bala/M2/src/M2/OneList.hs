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

  , toListF
  , accumMapL
   
  ) where


import Data.Semigroup           -- package: algebra

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable


infixr 5 :<

data OneList a = One a | a :< OneList a
  deriving (Eq)

instance Show a => Show (OneList a) where
  show = ('{':) . ($ []) . step where
     step (One a)   = shows a . showChar '}'
     step (a :< xs) = shows a . showChar ',' . step xs


instance Functor OneList where
  fmap f (One a)        = One $ f a
  fmap f (a :< as)      = f a :< fmap f as

instance Foldable OneList where
  foldMap f (One a)     = f a
  foldMap f (a :< as)   = f a `mappend` foldMap f as

  foldr f b0 = step b0 where
    step b (One a)   = f a b
    step b (a :< as) = f a (step b as)

  foldl f b0 = step b0 where
    step b (One a)   = f b a
    step b (a :< as) = step (f b a) as


instance Traversable OneList where
  traverse f (One a)    = One  <$> f a
  traverse f (a :< as)  = (:<) <$> f a <*> traverse f as


instance Semigroup (OneList e) where
  (One x)   `append` ys  = x :< ys
  (x :< xs) `append` ys  = x :< (xs `append` ys)



toListF :: (a -> b) -> OneList a -> [b]
toListF f = step where
  step (One x) = [f x]
  step (x :< xs) = f x : step xs


accumMapL :: (x -> st -> (y,st)) -> OneList x -> st -> (OneList y,st)
accumMapL f (One x)   st = let (y,st') = f x st in (One y,st')
accumMapL f (x :< xs) st = (y :< ys,st'')
                           where (y, st')  = f x st
                                 (ys,st'') = accumMapL f xs st'

