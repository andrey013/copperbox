{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.JoinList
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A Join List datatype and operations. 
--
-- Join Lists are symmetric lists, unlike the standard list 
-- datatype which is a cons list and builds by prefixing. 
--
--------------------------------------------------------------------------------


module Data.JoinList 
  ( 
    JoinList

  , empty
  , null
  , wrap
  , ( ++ )
  , concat
  , replicate
  , repeated
  , gfold

  , toList
  , fromList
  ) where


import Control.Applicative hiding ( empty )
import Data.Monoid
import Data.Traversable

import qualified Data.Foldable as F
import qualified Data.List     as L

import Prelude hiding ( (++), null, head, tail, concat, replicate )


data JoinList a = Empty 
                | Single a 
                | Join (JoinList a) (JoinList a) 
  deriving (Eq)

-- Bananaize !
instance Show a => Show (JoinList a) where
  showsPrec p xs = showChar '(' . showsPrec p (toList xs) . showChar ')'

instance Monoid (JoinList a) where
  mempty  = Empty
  mappend = (++)

instance Functor JoinList where
  fmap _ Empty = Empty
  fmap f (Single a) = Single (f a)
  fmap f (Join a b) = (fmap f a) ++ (fmap f b) 


instance Monad JoinList where
  return = Single
  Empty    >>= _ = Empty
  Single a >>= k = k a
  Join t u >>= k = Join (concat $ fmap k t) (concat $ fmap k u)


instance F.Foldable JoinList where
  foldMap _ Empty      = mempty
  foldMap f (Single a) = f a
  foldMap f (Join t u) = F.foldMap f t `mappend` F.foldMap f u


instance Traversable JoinList where
  traverse _ Empty      = pure Empty
  traverse f (Single a) = Single <$> f a
  traverse f (Join t u) = Join <$> traverse f t <*> traverse f u


empty :: JoinList a
empty = Empty

null :: JoinList a -> Bool
null Empty = True
null _     = False


wrap :: a -> JoinList a
wrap = Single

infixr 5 ++

(++) :: JoinList a -> JoinList a -> JoinList a
Empty ++ ys    = ys
xs    ++ Empty = xs
xs    ++ ys    = Join xs ys

concat :: JoinList (JoinList a) -> JoinList a
concat = F.foldl' mappend mempty 


replicate :: Int -> a -> JoinList a
replicate n a | n > 0     = step (n-1) (Single a)
              | otherwise = Empty
  where
    step 0 xs = xs
    step i xs = step (i-1) $ Single a `Join` xs

repeated :: Int -> JoinList a -> JoinList a
repeated n xs | n > 0     = step (n-1) xs
              | otherwise = Empty
  where
    step 0 ys = ys
    step i ys = step (i-1) $ xs `Join` ys


-- Jeremy Gibbons style general fold where each constructor has
-- an operation.
gfold :: b                              -- param e, replaces Empty
      -> (a -> b)                       -- param f, replaces Single
      -> (b -> b -> b)                  -- param g, replaces Join
      -> JoinList a 
      -> b
gfold e _ _ Empty      = e
gfold _ f _ (Single a) = f a
gfold e f g (Join t u) = g (gfold e f g t) (gfold e f g u)



toList :: JoinList a -> [a]
toList = F.foldr (:) []

fromList :: [a] -> JoinList a
fromList []     = Empty
fromList (x:xs) = L.foldl' (\a e -> a `Join` (Single e)) (Single x) xs


