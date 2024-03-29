{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OneMany
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Option type representing one or many.
--
-----------------------------------------------------------------------------

module Data.OneMany
  ( 
     OneMany
  , one
  , many

  , oneMany
  , cons

  , isMany
  , isOne

  , toList 
  , fromList

  , catOnes
  , catManys

  , separate

  ) where

import Control.Applicative hiding ( many )
import Data.Foldable ( Foldable, foldMap )
import Data.Traversable

data OneMany a = One a | Many [a]
  deriving (Eq,Show)

-- No natural Monoid instance for OneMany.

instance Functor OneMany where
  fmap f (One x)   = One (f x)
  fmap f (Many xs) = Many (fmap f xs) 

instance Foldable OneMany where
  foldMap f (One x)       = f x
  foldMap f (Many xs)     = foldMap f xs

instance Traversable OneMany where
  traverse f (One x)      = One <$> f x
  traverse f (Many xs)    = Many <$> traverse f xs


-- | Construct One.
one :: a -> OneMany a
one = One

-- | Construct Many. Not this function throws a error if the list has
-- zero or one elements
many :: [a] -> OneMany a
many []   = error "OneMany.many: cannot build Many from empty list"
many [_a] = error "OneMany.many: cannot build Many from singleton list"
many xs   = Many xs


-- | Case analysis for the OneMany type (c.f @either@ on the Either type or 
-- @maybe@ on the Maybe type). 
oneMany :: (a -> b) -> ([a] -> b) -> OneMany a -> b
oneMany f _ (One x)   = f x
oneMany _ g (Many xs) = g xs

-- | Prepend an element. Obviously this transforms a One to a Many.
cons :: a -> OneMany a -> OneMany a
cons x (One y)   = Many [x,y]
cons x (Many xs) = Many (x:xs)


isMany :: OneMany a -> Bool
isMany (Many _) = True
isMany _        = False

isOne :: OneMany a -> Bool
isOne (One _)   = True
isOne _         = False


toList :: OneMany a -> [a]
toList (One x)   = [x]
toList (Many xs) = xs

fromList :: [a] -> OneMany a
fromList []  = error "OneMany.fromList: cannot build One or Many from empty list"
fromList [x] = One x
fromList xs  = Many xs


catOnes :: [OneMany a] -> [a]
catOnes ls = [x | One x <- ls]


catManys :: [OneMany a] -> [[a]]
catManys ls = [xs | Many xs <- ls]


-- | Separate the ones from the manys.
separate :: [OneMany a] -> ([a],[[a]])
separate = foldr fn ([],[]) where
  fn (One x)   (ys,yss) = (x:ys,yss)
  fn (Many xs) (ys,yss) = (ys,xs:yss)