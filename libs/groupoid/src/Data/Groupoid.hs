{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Groupoid
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  FlexibleInstances
--
-- Groupoid - a set with a binary operator, more general than 
-- monoid as there is no obligation to have a neutral element 
-- (i.e mempty in Data.Monoid).
--
-----------------------------------------------------------------------------

module Data.Groupoid
  ( 
  -- * Groupoid typeclass   
     Groupoid(..)
  ) where

import Data.Monoid

class Groupoid a where
  gappend :: a -> a -> a
  -- ^ A binary operation, not necessarily associative.
  gconcat :: [a] -> a
  -- ^ Fold a non-empty list with the groupoid. The default 
  -- definition uses 'foldr1' which throws an exception when
  -- applied to the empty list.
  
  gconcat = foldr1 gappend


-- Groupoid instances

instance Groupoid [a] where
  gappend = (++)

instance Groupoid (a -> a) where
  gappend = (.)

instance Groupoid () where
  _ `gappend` _ = ()

instance (Groupoid a, Groupoid b) => Groupoid (a,b) where
  (a1,b1) `gappend` (a2,b2) = (a1 `gappend` a2, b1 `gappend` b2)

instance (Groupoid a, Groupoid b, Groupoid c) => Groupoid (a,b,c) where
  (a1,b1,c1) `gappend` (a2,b2,c2) = 
      (a1 `gappend` a2, b1 `gappend` b2, c1 `gappend` c2)


instance (Groupoid a, Groupoid b, Groupoid c, Groupoid d) => 
          Groupoid (a,b,c,d) where
  (a1,b1,c1,d1) `gappend` (a2,b2,c2,d2) = 
      (a1 `gappend` a2, b1 `gappend` b2, c1 `gappend` c2, d1 `gappend` d2)


instance (Groupoid a, Groupoid b, Groupoid c, Groupoid d, Groupoid e) => 
          Groupoid (a,b,c,d,e) where
  (a1,b1,c1,d1,e1) `gappend` (a2,b2,c2,d2,e2) = 
      (a1 `gappend` a2, b1 `gappend` b2, c1 `gappend` c2, 
                        d1 `gappend` d2, e1 `gappend` e2)

-- lexicographical ordering
instance Groupoid Ordering where
        LT `gappend` _ = LT
        EQ `gappend` y = y
        GT `gappend` _ = GT


-- Dual with swapping - as per Data.Monoid
instance Groupoid a => Groupoid (Dual a) where
  Dual x `gappend` Dual y = Dual (y `gappend` x)

instance Groupoid (Endo a) where
  gappend = mappend

instance Groupoid All where
  gappend = mappend

instance Groupoid Any where
  gappend = mappend

instance Num a => Groupoid (Sum a) where
  gappend = mappend

instance Num a => Groupoid (Product a) where
  gappend = mappend


instance Groupoid a => Groupoid (Maybe a) where
  Nothing `gappend` m       = m
  m       `gappend` Nothing = m
  Just m1 `gappend` Just m2 = Just (m1 `gappend` m2) 


instance Groupoid (First a) where
  r@(First (Just _)) `gappend` _ = r
  First Nothing      `gappend` r = r

instance Groupoid (Last a) where
  _ `gappend` r@(Last (Just _)) = r
  r `gappend` Last Nothing      = r


