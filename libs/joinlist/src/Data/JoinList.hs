{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.JoinList
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A Join List datatype and operations. 
--
-- Join Lists are symmetric lists where joining two lists 
-- (catenation, aka (++)) is a cheap operation. This constrasts 
-- with the regular list datatype which is a cons list: while 
-- consing on a regular list (building a list by adding an 
-- elementary prefix) is by nature cheap, joining (++) is 
-- expensive. 
--
--------------------------------------------------------------------------------


module Data.JoinList 
  ( 
  -- * Join list datatype
    JoinList

  -- * Elementary construction  
  , empty
  , wrap
  , ( ++ )
  , join

  -- * Basic functions  
  , null
  , concat
  , length
  , map

  -- * Building join lists
  , replicate
  , repeated

  -- * Generalized fold
  , gfold

  -- * Zipping
  , xzip
  , xzipWith

  -- * Conversion between join lists and regular lists
  , toList
  , fromList
  
  ) where


import Control.Applicative hiding ( empty )
import Data.Monoid
import Data.Traversable
import Text.Show ( showListWith )

import qualified Data.Foldable as F
import qualified Data.List     as L

import Prelude hiding ( (++), null, length, map, concat, replicate )


data JoinList a = Empty 
                | Single a 
                | Join (JoinList a) (JoinList a) 
  deriving (Eq)

-- Bananaize !
instance Show a => Show (JoinList a) where
  showsPrec _ xs = showChar '(' . showListWith shows (toList xs) . showChar ')'

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

--------------------------------------------------------------------------------
-- Construction

-- | Create an empty join list. 
empty :: JoinList a
empty = Empty

-- | Create a singleton join list.
wrap :: a -> JoinList a
wrap = Single


infixr 5 ++

-- | Catenate two join lists. Unlike (++) on regular lists, 
-- catenation on join lists is (relatively) cheap hence the 
-- monicker /join list/.
(++) :: JoinList a -> JoinList a -> JoinList a
Empty ++ ys    = ys
xs    ++ Empty = xs
xs    ++ ys    = Join xs ys


infixr 5 `join`

-- | An alias for (++) that does not cause a name clash with the 
-- Prelude.
join :: JoinList a -> JoinList a -> JoinList a
join = (++)


--------------------------------------------------------------------------------
-- Basic functions

-- | Test whether a join list is empty.
null :: JoinList a -> Bool
null Empty = True
null _     = False

-- | Concatenate a (join) list of (join) lists. 
concat :: JoinList (JoinList a) -> JoinList a
concat = F.foldl' mappend mempty 

-- | Get the length of a join list.
length :: JoinList a -> Int
length = gfold 0 (const 1) (+)

-- | Map a function over a join list.
map :: (a -> b) -> JoinList a -> JoinList b
map = fmap

--------------------------------------------------------------------------------
-- Building join lists

-- | Build a join list of n elements. 
replicate :: Int -> a -> JoinList a
replicate n a | n > 0     = step (n-1) (Single a)
              | otherwise = Empty
  where
    step 0 xs = xs
    step i xs = step (i-1) $ Single a `Join` xs

-- | Repeatedly build a join list by catenating the seed list.
repeated :: Int -> JoinList a -> JoinList a
repeated n xs | n > 0     = step (n-1) xs
              | otherwise = Empty
  where
    step 0 ys = ys
    step i ys = step (i-1) $ xs `Join` ys

--------------------------------------------------------------------------------
-- Generalized fold

-- | A Jeremy Gibbons style generalised fold, where each constructor has
-- an operation.
gfold :: b                              -- param e, replaces Empty
      -> (a -> b)                       -- param f, replaces Single
      -> (b -> b -> b)                  -- param g, replaces Join
      -> JoinList a 
      -> b
gfold e _ _ Empty      = e
gfold _ f _ (Single a) = f a
gfold e f g (Join t u) = g (gfold e f g t) (gfold e f g u)


--------------------------------------------------------------------------------
-- Zipping

-- | /cross zip/ - zip a join list against a regular list, 
-- maintaining the shape of the join list provided the lengths 
-- of the lists match.
xzip :: JoinList a -> [b] -> JoinList (a,b)
xzip = xzipWith (,)

-- | Generalized cross zip - c.f. zipWith on regular lists.
xzipWith :: (a -> b -> c) -> JoinList a -> [b] -> JoinList c
xzipWith fn xs0 ys0       = fst $ step xs0 ys0
  where 
   step Empty      xs     = (Empty,xs)
   step (Single a) (x:xs) = (Single (fn a x),xs)
   step (Single _) []     = (Empty,[])
   step (Join t u) xs     = (Join t' u',xs'') where
                              (t',xs')  = step t xs
                              (u',xs'') = step u xs'

--------------------------------------------------------------------------------
-- Conversion

-- | Convert a join list to a regular list.
toList :: JoinList a -> [a]
toList = F.foldr (:) []

-- | Build a join list from a regular list.
fromList :: [a] -> JoinList a
fromList []     = Empty
fromList (x:xs) = L.foldl' (\a e -> a `Join` (Single e)) (Single x) xs


