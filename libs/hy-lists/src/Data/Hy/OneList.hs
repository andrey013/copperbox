{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Hy.OneList
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
-- 
-- Non-empty OneList
--
--------------------------------------------------------------------------------

module Data.Hy.OneList 
  ( 
    OneList(..)
  
  , fromList
  , toList

  , one
  , singleton
  , head 
  , tail

  , map
  , foldr
  , foldl

  ) where

import Data.Hy.DListDisguise

import Control.Applicative              hiding ( empty )
import Data.Foldable ( Foldable )
import qualified Data.Foldable          as F
import qualified Data.List              as List
import Data.Monoid
import Data.Traversable ( Traversable(..) )

import Prelude hiding ( concat, foldl, foldr, head, length, map, tail)

infixr 5 :+

data OneList a = One a | a :+ OneList a
  deriving (Eq)



--------------------------------------------------------------------------------


instance Show a => Show (OneList a) where
  show = ('{' :) . List.concat . dlout . step where
     step (One a)   = (dlwrap $ show a) ++++ dlwrap "}"
     step (a :+ xs) = (dlwrap $ show a) ++++ dlwrap "," ++++ step xs

--------------------------------------------------------------------------------

instance Functor OneList where
  fmap = map 


-- Snoc lists are \'naturally consumed\' from the beginning 
-- i.e. left-fold rather than right-fold.

instance Foldable OneList where
  foldl               = foldl
  foldr               = foldr  

  foldMap f (One a)   = f a
  foldMap f (a :+ xs) = f a `mappend` F.foldMap f xs


instance Traversable OneList where
  traverse f (One a)   = One  <$> f a
  traverse f (a :+ xs) = (:+) <$> f a <*> traverse f xs






--------------------------------------------------------------------------------


-- | Convert a standard (cons, prefix) list to a one list. 
-- This function fails with error if the input list is empty.
--
fromList :: [a] -> OneList a
fromList []     = error "Data.Hy.OneList.fromList - empty list."
fromList [a]    = One a
fromList (x:xs) = x :+ fromList xs

-- | Convert a Snoc list to a standard (cons, prefix) list.. 
toList :: OneList a -> [a]
toList (One a)   = [a]
toList (a :+ xs) = a : toList xs


-- | c.f null
one :: OneList a -> Bool
one (One _) = True
one _       = False

singleton :: a -> OneList a
singleton = One 


-- | Extract the first element - for OneList\'s @head@ is 
-- naturally total.
head :: OneList a -> a
head (One a)  = a
head (a :+ _) = a

-- | Extract the rest of the list after the first element. 
-- This function fails with error if the list has a single 
-- element.
tail :: OneList a -> OneList a
tail (_ :+ xs) = xs
tail _         = error "Data.Hy.OneList.tail - single element list, no tail."

map :: (a -> b) -> OneList a -> OneList b
map f (One a)   = One (f a)
map f (a :+ xs) = f a :+ map f xs


-- No direct unfoldr - there is no equivalent to [] to produce 
-- on the Nothing case

foldr :: (a -> b -> b) -> b -> OneList a -> b
foldr f b (One a)   = f a b
foldr f b (x :+ xs) = f x (foldr f b xs)

foldl :: (b -> a -> b) -> b -> OneList a -> b
foldl f b (One a)   = f b a
foldl f b (a :+ xs) = foldl f (f b a) xs




-- paramorphism - impossible?
-- > para :: (a -> (OneList a, b) -> b) -> b -> OneList a -> b
--
-- Cannot handle the terminal One case.
--
-- Drop a - bad
--
-- > para f e (One a) = e
--
-- Nothing to put in the gap:
--
-- > para f e (One a) = f a ( _??_ ,e)
--

