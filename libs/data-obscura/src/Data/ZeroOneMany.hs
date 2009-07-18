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
  , zero 
  , one
  , many

  , zeroOneMany
  , cons

  , isMany
  , isOne
  , isZero

  , toList 
  , fromList

  , catOnes
  , catManys

  , separate

  ) where

import Control.Applicative hiding ( many )
import Data.Foldable ( Foldable, foldMap )
import Data.Monoid
import Data.Traversable

data ZeroOneMany a = Zero | One a | Many [a]
  deriving (Eq,Show)


instance Monoid (ZeroOneMany a) where
  mempty = Zero
  (Many xs) `mappend` (Many ys) = Many (xs ++ ys)
  (Many xs) `mappend` (One y)   = Many (xs++[y])
  (One x)   `mappend` y         = cons x y
  Zero      `mappend` b         = b
  a         `mappend` Zero      = a


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



-- | Construct Zero.
zero :: ZeroOneMany a
zero = Zero


-- | Construct One.
one :: a -> ZeroOneMany a
one = One

-- | Construct Many. Not this function throws a error if the list has
-- zero or one elements
many :: [a] -> ZeroOneMany a
many []   = error "ZeroOneMany.many: cannot build Many from empty list"
many [_x] = error "ZeroOneMany.many: cannot build Many from singleton list"
many xs   = Many xs



-- | Case analysis for the ZeroOneMany type (c.f @either@ on the Either 
-- type or @maybe@ on the Maybe type). 
zeroOneMany :: b -> (a -> b) -> ([a] -> b) -> ZeroOneMany a -> b
zeroOneMany b _ _  Zero     = b
zeroOneMany _ f _ (One x)   = f x
zeroOneMany _ _ g (Many xs) = g xs


-- | Prepend an element. Obviously this transforms a Zero to a One and a
-- One to a Many.
cons :: a -> ZeroOneMany a -> ZeroOneMany a
cons x Zero      = One x
cons x (One y)   = Many [x,y]
cons x (Many xs) = Many (x:xs)



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