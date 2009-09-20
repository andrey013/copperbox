{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Kleene
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Grammatical view...
--
--------------------------------------------------------------------------------

module Bala.Kleene where

import Bala.Utils ( iter )

import Control.Applicative
import qualified Data.Foldable as F
import Data.Monoid
import Data.Traversable

-- cf Kleene algebra / regular expression
data Kleene a = E                                 -- Empty
              | O a                               -- One
              | S (Kleene a) (Kleene a)           -- Sequence
              | R Int (Kleene a)                  -- Repetition
              | A Int (Kleene a) (Kleene a)       -- Alternation
  deriving (Eq,Show)

instance Functor Kleene where
  fmap _ E         = E
  fmap f (O a)     = O (f a)
  fmap f (S t u)   = S (fmap f t) (fmap f u)
  fmap f (R i t)   = R i (fmap f t)
  fmap f (A i t u) = A i (fmap f t) (fmap f u)

instance F.Foldable Kleene where
  foldMap _ E         = mempty
  foldMap f (O a)     = f a
  foldMap f (S t u)   = F.foldMap f t `mappend` F.foldMap f u
  foldMap f (R i t)   = iter i (mappend (F.foldMap f t)) mempty
  foldMap f (A i t u) = mconcat $ alternate i (F.foldMap f t, F.foldMap f u) 


instance Traversable Kleene where
  traverse _ E         = pure E
  traverse f (O a)     = O <$> f a
  traverse f (S t u)   = S <$> traverse f t <*> traverse f u
  traverse f (R i t)   = R i <$> traverse f t
  traverse f (A i t u) = A i <$> traverse f t <*> traverse f u 

alternate :: Int -> (a,a) -> [a]
alternate i (a,b) | i <= 0    = []
                  | i == 1    = [a]
                  | otherwise = a : b : alternate (i-2) (a,b)

-- TODO - should be faster to write own implementation
toList :: Kleene a -> [a]
toList = F.toList


-- Note - no minimization is performed 
fromList :: [a] -> Kleene a
fromList []     = E
fromList [a]    = O a
fromList (x:xs) = S (O x) (fromList xs)


-- Gibbons style general folds
gfold :: b                              -- param e, replaces E
      -> (a -> b)                       -- param f, replaces O
      -> (b -> b -> b)                  -- param g, replaces S
      -> (Int -> b -> b)                -- param h, replaces R
      -> (Int -> b -> b -> b)           -- param i, replaces A
      -> Kleene a 
      -> b
gfold e _ _ _ _ E         = e
gfold _ f _ _ _ (O a)     = f a
gfold e f g h i (S t u)   = g (gfold e f g h i t) (gfold e f g h i u)
gfold e f g h i (R n t)   = h n (gfold e f g h i t)
gfold e f g h i (A n t u) = i n (gfold e f g h i t) (gfold e f g h i u)


repeatn :: Int -> Kleene a -> Kleene a
repeatn n m = R n m

