{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Hy.SnocList
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
-- 
-- Snoc List
--
--------------------------------------------------------------------------------

module Data.Hy.SnocList
 (
   SnocList(..)
 -- * Construction
 , fromList
 , toList

 -- * Basic functions
 , empty
 , singleton
 , snoc
 , null
 , last
 , append
 , unfoldl
 , foldl
 , foldr
 , concat
 , length
 , para
 
 )  where

import Data.Hy.DListDisguise


import Control.Applicative              hiding ( empty )
import Control.Monad ( MonadPlus(..) )
import Data.Foldable ( Foldable )
import qualified Data.Foldable          as F
import Data.Monoid
import qualified Data.List              as List
import Data.Traversable ( Traversable(..) )

import Prelude hiding ( concat, foldl, foldr, last, length, map, null )

-- ARE YOU SURE ?

infixl 5 :> 

data SnocList a = Lin | SnocList a :> a
  deriving (Eq)


--------------------------------------------------------------------------------

instance Show a => Show (SnocList a) where
  show = List.concat . dlout . (dlsnoc `flip`  ">") . para phi (dlwrap "<") where
     phi c (s,acc) | null s    = acc ++++ (dlwrap $ show c)
                   | otherwise = acc ++++ (dlwrap $ ',' : show c)

--------------------------------------------------------------------------------

instance Functor SnocList where
  fmap = map

-- Snoc lists are \'naturally consumed\' from the beginning 
-- i.e. left-fold rather than right-fold.

instance Foldable SnocList where
  foldl               = foldl
  foldr               = foldr  

  foldMap _ Lin       = mempty
  foldMap f (sc :> a) = F.foldMap f sc `mappend` (f a)

  

instance Traversable SnocList where
  traverse _ Lin       = pure Lin
  traverse f (sc :> a) = (:>) <$> traverse f sc <*> f a

--------------------------------------------------------------------------------

instance Monad SnocList where
  m >>= k = concat (fmap k m)
  return  = singleton
  fail _x = Lin

instance MonadPlus SnocList where
  mzero = Lin
  Lin `mplus` b = b
  a   `mplus` _ = a

instance Monoid (SnocList a) where
  mempty  = Lin
  mappend = append


--------------------------------------------------------------------------------

-- | Convert a standard (cons, prefix) list to a Snoc list. 
fromList :: [a] -> SnocList a
fromList = List.foldl' (flip snoc) empty

-- | Convert a Snoc list to a standard (cons, prefix) list.. 
toList :: SnocList a -> [a]
toList = F.toList

-- | Create an empty Snoc list.
empty :: SnocList a 
empty = Lin

-- | Create a one element Snoc list from the supplied argument.
singleton :: a -> SnocList a
singleton a = snoc a Lin


-- | Suffix an element to the right-end of the Snoc list.
snoc :: a -> SnocList a -> SnocList a
snoc a Lin = Lin :> a
snoc a s   = s :> a

-- | Test for the empty Snoc list.
null :: SnocList a -> Bool
null Lin = True
null _   = False

-- | Extract the last element of the list. 
--
-- This function is naturally partial and throws an error on 
-- the empty list.
last :: SnocList a -> a
last Lin      = error "Data.SnocList.last: empty list"
last (_ :> a) = a 


append :: SnocList a -> SnocList a -> SnocList a
append xs Lin      = xs
append xs (sc :> a) = (xs `append` sc) :> a


map :: (a -> b) -> SnocList a -> SnocList b
map _ Lin       = Lin
map f (sc :> a) = map f sc :> f a


unfoldl :: (b -> Maybe (a, b)) -> b -> SnocList a
unfoldl f = step where
  step st = case (f st) of
              Nothing -> Lin
              Just (a,st') -> step st' :> a

foldl :: (b -> a -> b) -> b -> SnocList a -> b
foldl _ e Lin       = e
foldl f e (sc :> a) = f (foldl f e sc) a 


foldr :: (a -> b -> b) -> b -> SnocList a -> b
foldr _ e Lin       = e
foldr f e (sc :> a) = foldr f (f a e) sc

concat :: SnocList (SnocList a) -> SnocList a
concat = foldr append Lin

length :: SnocList a -> Int
length = foldl (const . (+1)) 0

-- | Paramorphism - a paramorphism is a generalization of fold.
-- Like a fold the list is consumed element-wise, but a 
-- paramorphism can view the /rest-of-the-list/ at each step.
para :: (a -> (SnocList a, b) -> b) -> b -> SnocList a -> b
para _ e Lin       = e
para f e (sc :> a) = f a (sc,para f e sc)
