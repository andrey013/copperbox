{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.OneList
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
-- 
--
--
--------------------------------------------------------------------------------

module Data.OneList 
  ( 
    OneList(..)

  , one
  , singleton
  , head 
  , tail

  , map
  , foldr
  , foldl

  ) where

import Data.DListDisguise

import qualified Data.List as List

import Prelude hiding ( concat, foldl, foldr, head, length, map, tail)

infixr 5 :* 

data OneList a = One a | a :* OneList a
  deriving (Eq)



--------------------------------------------------------------------------------


instance Show a => Show (OneList a) where
  show = ('{' :) . List.concat . dlout . step where
     step (One a)   = (dlwrap $ show a) ++++ dlwrap "}"
     step (a :* xs) = (dlwrap $ show a) ++++ dlwrap "," ++++ step xs

--------------------------------------------------------------------------------

instance Functor OneList where
  fmap = map 



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
head (a :* _) = a

-- | Extract the rest of the list after the first element. 
-- This function fails with error if the list has a single 
-- element.

tail :: OneList a -> OneList a
tail (_ :* xs) = xs
tail _         = error $ "Data.OneList.tail - single element list, no tail."

map :: (a -> b) -> OneList a -> OneList b
map f (One a)   = One (f a)
map f (a :* xs) = f a :* map f xs


-- No direct unfoldr - there is no equivalent to [] to produce 
-- on the Nothing case

foldr :: (a -> b -> b) -> b -> OneList a -> b
foldr f b (One a)   = f a b
foldr f b (x :* xs) = f x (foldr f b xs)

foldl :: (b -> a -> b) -> b -> OneList a -> b
foldl f b (One a)   = f b a
foldl f b (a :* xs) = foldl f (f b a) xs




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
