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

  , oneListR
  , oneListU
   
  ) where


import Data.Semigroup           -- package: algebra
import Language.KURE            -- package: kure

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Traversable
import qualified Data.Traversable   as T


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




oneListR    :: (Monoid dec, Monad m) 
            => Rewrite m dec a 
            -> Rewrite m dec (OneList a)
oneListR rr = transparently $ rewrite $ T.mapM (apply rr)

oneListU    :: (Monoid dec, Monad m, Monoid r) 
            => Translate m dec a r 
            -> Translate m dec (OneList a) r
oneListU rr = translate $ liftM leftMerge . T.mapM (apply rr) 

    
leftMerge :: (Foldable f, Monoid a) => f a -> a
leftMerge = foldl' mappend mempty
