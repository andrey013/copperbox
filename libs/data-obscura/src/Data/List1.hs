{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List1
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Non-empty lists
--
-----------------------------------------------------------------------------

module Data.List1 
  (
  -- * Non-empty list type  
    List1(..)
  
  -- * Construction and deconstruction
  , fromList
  , toList
  , singleton

  -- * Basic functions
  , head
  , last
  , tail
  , single
  , length
  
  -- * Transformations
  , reverse

  -- * Common functionals
  , map
  , zip
  , zipWith
  )
  where


import Control.Applicative
import Data.Foldable ( Foldable, foldMap )
import Data.Monoid
import Data.Traversable

import Prelude hiding ( head, last, tail, length, reverse, map, zip, zipWith )

infixr 5 :<<

data List1 a = Singleton a
             | a :<< (List1 a)


instance Show a => Show (List1 a) where
  showsPrec i xs = showString "fromList " . showsPrec i (toList xs)

instance Functor List1 where
  fmap f (Singleton a) = Singleton (f a)
  fmap f (a :<< ne)    = (f a) :<< (fmap f ne)


instance Foldable List1 where
  foldMap f (Singleton a) = f a
  foldMap f (a :<< ne)    = f a `mappend` foldMap f ne

instance Traversable List1 where
  traverse f (Singleton a) = Singleton <$> f a
  traverse f (a :<< ne)    = (:<<) <$> f a <*> traverse f ne


-- | Convert a standard list into a non-empty list. 
-- /Beware/ attempting to convert an empty list throws an error.
fromList :: [a] -> List1 a
fromList [x]    = Singleton x
fromList (x:xs) = x :<< (fromList xs)
fromList []     = error "List1.fromList applied to empty list"

-- | Convert a non-empty list to a standard list.
toList :: List1 a -> [a]
toList (Singleton a) = [a]
toList (a :<< ne)  = a : toList ne 

-- | Create a non-empty list from a single element.
singleton :: a -> List1 a
singleton = Singleton


-- | Extract the first element - this is total of course.
head :: List1 a -> a
head (Singleton a) = a
head (a :<< _)   = a

-- | Extract the last element - this is total of course.
last :: List1 a -> a
last (Singleton a) = a
last (_ :<< ne)  = last ne

-- | Extract the tail of a list. This function is partial 
-- and throws an error if performed on a singleton.
tail :: List1 a -> List1 a
tail (Singleton _) = error "List1.tail: tail of a singleton."
tail (_ :<< ne)  = ne


-- | Test whether non-empty list has only one element.
single :: List1 a -> Bool
single (Singleton _) = True
single _             = False


-- | Get the length of a finite non-empty list.
length :: List1 a -> Int
length (Singleton _) = 1
length (_ :<< ne)  = 1 + length ne


reverse :: List1 a -> List1 a
reverse (Singleton a) = Singleton a
reverse (a :<< ne)    = work ne (singleton a) where
  work (Singleton x) ys = x :<< ys
  work (x :<< xs)    ys = work xs (x :<< ys)


-- | Standard map functional.
map :: (a -> b) -> List1 a -> List1 b
map = fmap

-- | Zip two non-empty lists. Note - if the list are not the same 
-- length the longer one will be truncated.
zip :: List1 a -> List1 b -> List1 (a,b)
zip = zipWith (,)

-- | Zip two non-empty lists with a custom operator.
zipWith :: (a -> b -> c) -> List1 a -> List1 b -> List1 c
zipWith f (a :<< na)    (b :<< nb)    = f a b :<< zipWith f na nb 
zipWith f a             b             = Singleton $ f (head a) (head b)



