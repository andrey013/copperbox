{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.OneList
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data type for non-empty lists.
-- 
-- Structurally the same as OneMany - but used for a different
-- purpose.
--
--------------------------------------------------------------------------------

module Wumpus.Core.OneList
  (
   -- OneMany
    OneList
  , ViewOL(..)

  , one
  , cons
  , head
  , viewl

  , fromList

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

import Prelude hiding ( head )

-- type OneMany a = OneList a

data OneList a = One a | Many a (OneList a)
  deriving (Eq)

data ViewOL a = OneL a | a :<< (OneList a)
  deriving (Eq)

--------------------------------------------------------------------------------
-- Instances

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

--------------------------------------------------------------------------------
-- | Construct One.
one :: a -> OneList a
one = One


-- | Prepend an element. Obviously this transforms a One to a Many.
cons :: a -> OneList a -> OneList a
cons a as   = Many a as

-- | 'head' is total of course.
head :: OneList a -> a
head (One a)    = a
head (Many a _) = a

viewl :: OneList a -> ViewOL a
viewl (One a)     = OneL a
viewl (Many a as) = a :<< as

-- | Construct Many. Not this function throws a error if the list has
-- zero or one elements
fromList :: [a] -> OneList a
fromList []     = error "OneList.fromList: cannot build Many from empty list"
fromList [a]    = One a
fromList (a:as) = Many a (fromList as)


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

