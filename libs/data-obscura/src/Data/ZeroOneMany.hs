{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ZeroOneMany
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Option type representing zero, one or many.
--
-----------------------------------------------------------------------------

module Data.ZeroOneMany
  ( 
     ZeroOneMany

  , summary

  , isMany
  , isOne
  , isZero

  , toList 
  , fromList

  , catOnes
  , catManys

  , separate

  ) where

import Control.Applicative
import Data.Foldable ( Foldable, foldMap )
import Data.Monoid
import Data.Traversable

data ZeroOneMany a = Zero | One a | Many [a]
  deriving (Eq,Show)


instance Functor ZeroOneMany where
  fmap _ Zero      = Zero
  fmap f (One x)   = One (f x)
  fmap f (Many xs) = Many (fmap f xs) 

instance Foldable ZeroOneMany where
  foldMap _ Zero          = mempty
  foldMap f (One x)       = f x
  foldMap f (Many xs)     = foldMap f xs

instance Traversable ZeroOneMany where
  traverse _ Zero         = pure Zero
  traverse f (One x)      = One <$> f x
  traverse f (Many xs)    = Many <$> traverse f xs


-- | Reduce one or many elements to a summary value, return the 
-- default value if Zero.
summary :: (a -> b) -> ([a] -> b) -> b -> ZeroOneMany a -> b
summary _ _ b Zero    = b
summary f _ _ (One x)   = f x
summary _ g _ (Many xs) = g xs

isMany :: ZeroOneMany a -> Bool
isMany (Many _) = True
isMany _        = False

isOne :: ZeroOneMany a -> Bool
isOne (One _) = True
isOne _       = False

isZero :: ZeroOneMany a -> Bool
isZero Zero = True
isZero _    = False


toList :: ZeroOneMany a -> [a]
toList Zero      = []
toList (One x)   = [x]
toList (Many xs) = xs

fromList :: [a] -> ZeroOneMany a
fromList []  = Zero
fromList [x] = One x
fromList xs  = Many xs


catOnes :: [ZeroOneMany a] -> [a]
catOnes ls = [x | One x <- ls]


catManys :: [ZeroOneMany a] -> [[a]]
catManys ls = [xs | Many xs <- ls]


-- | Separate the ones from the manys, discard the zeros.
separate :: [ZeroOneMany a] -> ([a],[[a]])
separate = foldr fn ([],[]) where
  fn Zero      (ys,yss) = (ys,yss)
  fn (One x)   (ys,yss) = (x:ys,yss)
  fn (Many xs) (ys,yss) = (ys,xs:yss)