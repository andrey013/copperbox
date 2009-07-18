{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ZeroMany
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Option type representing zero or many.
--
-----------------------------------------------------------------------------

module Data.ZeroMany
  ( 
     ZeroMany
  , zero
  , many

  , zeroMany
  , cons

  , isMany
  , isZero

  , toList 
  , fromList

  , catManys


  ) where

import Control.Applicative hiding ( many )
import Data.Foldable ( Foldable, foldMap )
import Data.Monoid
import Data.Traversable

data ZeroMany a = Zero | Many [a]
  deriving (Eq,Show)

instance Monoid (ZeroMany a) where
  mempty = Zero
  (Many xs) `mappend` (Many ys) = Many (xs ++ ys)
  Zero      `mappend` b         = b
  a         `mappend` Zero      = a
  

instance Functor ZeroMany where
  fmap _ Zero      = Zero
  fmap f (Many xs) = Many (map f xs) 

instance Foldable ZeroMany where
  foldMap _ Zero       = mempty
  foldMap f (Many xs)  = foldMap f xs

instance Traversable ZeroMany where
  traverse _ Zero      = pure Zero
  traverse f (Many xs) = Many <$> traverse f xs



-- | Construct Zero.
zero :: ZeroMany a
zero = Zero

-- | Construct Many. Not this function throws a error if the list has
-- zero elements
many :: [a] -> ZeroMany a
many []  = error "ZeroMany.many: cannot build Many from empty list"
many xs  = Many xs


-- | Case analysis for the ZeroMany type (c.f @either@ on the Either 
-- type or @maybe@ on the Maybe type). 
zeroMany :: b -> ([a] -> b) -> ZeroMany a -> b
zeroMany b _ Zero      = b
zeroMany _ f (Many xs) = f xs



-- | Prepend an element. Obviously this transforms a Zero to a Many.
cons :: a -> ZeroMany a -> ZeroMany a
cons x Zero      = Many [x]
cons x (Many xs) = Many (x:xs)


isMany :: ZeroMany a -> Bool
isMany (Many _) = True
isMany _        = False

isZero :: ZeroMany a -> Bool
isZero Zero = True
isZero _    = False


toList :: ZeroMany a -> [a]
toList Zero      = []
toList (Many xs) = xs

fromList :: [a] -> ZeroMany a
fromList []  = Zero
fromList xs  = Many xs



catManys :: [ZeroMany a] -> [[a]]
catManys ls = [xs | Many xs <- ls]


