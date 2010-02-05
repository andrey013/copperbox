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
-- Data type for one or many.
--
--------------------------------------------------------------------------------

module M2.OneList
  (
    OneMany
  , OneList
  , toListF
  , accumMapL
  , isOne
  , isMany
   
  ) where


import Data.Semigroup           -- package: algebra

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable


type OneMany a = OneList a

data OneList a = One a | Many a (OneList a)
  deriving (Eq)

instance Show a => Show (OneList a) where
  show = ('{':) . ($ []) . step where
     step (One a)     = shows a . showChar '}'
     step (Many a as) = shows a . showChar ',' . step as


instance Functor OneList where
  fmap f (One a)        = One $ f a
  fmap f (Many a as)    = Many (f a) (fmap f as)

instance Foldable OneList where
  foldMap f (One a)     = f a
  foldMap f (Many a as) = f a `mappend` foldMap f as

  foldr f b0 = step b0 where
    step b (One a)      = f a b
    step b (Many a as)  = f a (step b as)

  foldl f b0 = step b0 where
    step b (One a)      = f b a
    step b (Many a as)  = step (f b a) as


instance Traversable OneList where
  traverse f (One a)      = One  <$> f a
  traverse f (Many a as)  = Many <$> f a <*> traverse f as


instance Semigroup (OneList e) where
  (One a)     `append` bs  = Many a bs
  (Many a as) `append` bs  = Many a (as `append` bs)



toListF :: (a -> b) -> OneList a -> [b]
toListF f = step where
  step (One x)     = [f x]
  step (Many x xs) = f x : step xs


accumMapL :: (x -> st -> (y,st)) -> OneList x -> st -> (OneList y,st)
accumMapL f (One x)     st = let (y,st') = f x st in (One y,st')
accumMapL f (Many x xs) st = (Many y ys,st'')
                             where (y, st')  = f x st
                                   (ys,st'') = accumMapL f xs st'

isMany :: OneList a -> Bool
isMany (Many _ _) = True
isMany _          = False

isOne :: OneList a -> Bool
isOne (One _)     = True
isOne _           = False

