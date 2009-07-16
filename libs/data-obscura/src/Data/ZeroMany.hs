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


