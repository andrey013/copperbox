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
  -- * Join list datatype, opaque.
    JoinList

  -- * Conversion between join lists and regular lists
  , fromList
  , toList

  -- * Construction
  , empty
  , singleton
  , cons
  , snoc
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
  , foldr
  , foldl
  , unfoldl
  , unfoldr

  -- * Zipping
  , xzip
  , xzipWith

  
  ) where


import Control.Applicative hiding ( empty )

import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import Data.Monoid
import Data.Traversable ( Traversable(..) )

import Prelude hiding ( (++), concat, foldl, foldr, length
                      , map, null, replicate )


data JoinList a = Empty 
                | Single a 
                | JoinList a :++: JoinList a
  deriving (Eq,Show)



--------------------------------------------------------------------------------

instance Monoid (JoinList a) where
  mempty  = Empty
  mappend = (++)

instance Functor JoinList where
  fmap = map

instance Monad JoinList where
  return = Single
  Empty      >>= _ = Empty
  Single a   >>= k = k a
  (t :++: u) >>= k = (concat $ fmap k t) :++: (concat $ fmap k u)


instance Foldable JoinList where
  foldMap _ Empty      = mempty
  foldMap f (Single a) = f a
  foldMap f (t :++: u) = F.foldMap f t `mappend` F.foldMap f u

  foldr                = foldr
  foldl                = foldl

instance Traversable JoinList where
  traverse _ Empty      = pure Empty
  traverse f (Single a) = Single <$> f a
  traverse f (t :++: u) = (:++:) <$> traverse f t <*> traverse f u

--------------------------------------------------------------------------------
-- Conversion

-- | Convert a join list to a regular list.
toList :: JoinList a -> [a]
toList = foldl (flip (:)) []

-- | Build a join list from a regular list.
fromList :: [a] -> JoinList a
fromList []     = Empty
fromList [x]    = Single x
fromList (x:xs) = x `cons` fromList xs


--------------------------------------------------------------------------------

-- | Create an empty join list. 
empty :: JoinList a
empty = Empty

-- | Create a singleton join list.
singleton :: a -> JoinList a
singleton = Single


-- | Cons an element to the front of the join list.
cons :: a -> JoinList a -> JoinList a
cons a xs = singleton a ++ xs  

-- | Snoc an element to the tail of the join list.
snoc :: JoinList a -> a -> JoinList a
snoc xs a = xs ++ singleton a


infixr 5 ++

-- | Catenate two join lists. Unlike (++) on regular lists, 
-- catenation on join lists is (relatively) cheap hence the 
-- name /join list/.
(++) :: JoinList a -> JoinList a -> JoinList a
Empty ++ ys    = ys
xs    ++ Empty = xs
xs    ++ ys    = xs :++: ys


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
concat = foldl mappend mempty 

-- | Get the length of a join list.
length :: JoinList a -> Int
length = gfold 0 (const 1) (+)

-- | Map a function over a join list.
map :: (a -> b) -> JoinList a -> JoinList b
map _ Empty      = Empty
map f (Single a) = Single (f a)
map f (a :++: b) = (map f a) :++: (map f b) 


--------------------------------------------------------------------------------
-- Building join lists

-- | Build a join list of n elements. 
replicate :: Int -> a -> JoinList a
replicate n a | n > 0     = step (n-1) (Single a)
              | otherwise = Empty
  where
    step 0 xs = xs
    step i xs = step (i-1) $ Single a :++: xs

-- | Repeatedly build a join list by catenating the seed list.
repeated :: Int -> JoinList a -> JoinList a
repeated n xs | n > 0     = step (n-1) xs
              | otherwise = Empty
  where
    step 0 ys = ys
    step i ys = step (i-1) $ xs :++: ys

--------------------------------------------------------------------------------
-- Generalized fold

-- | A generalized fold, where each constructor has an operation.
gfold :: b                              -- param e, replaces Empty
      -> (a -> b)                       -- param f, replaces Single
      -> (b -> b -> b)                  -- param g, replaces Join
      -> JoinList a 
      -> b
gfold e _ _ Empty      = e
gfold _ f _ (Single a) = f a
gfold e f g (t :++: u) = g (gfold e f g t) (gfold e f g u)

-- | Right-associative fold of a JoinList.
foldr :: (a -> b -> b) -> b -> JoinList a -> b
foldr _ e Empty      = e
foldr f e (Single a) = f a e
foldr f e (t :++: u) = foldr f (foldr f e t) u


-- | Left-associative fold of a JoinList.
foldl :: (b -> a -> b) -> b -> JoinList a -> b
foldl _ e Empty      = e
foldl f e (Single a) = f e a
foldl f e (t :++: u) = foldl f (foldl f e u) t

-- | unfoldl is permitted due to cheap /snoc-ing/.
unfoldl :: (b -> Maybe (a, b)) -> b -> JoinList a
unfoldl f = step where
  step st = case f st of
              Nothing -> Empty
              Just (a,st') -> step st' `snoc` a

-- | unfoldr - the /usual/ unfoldr opertation.
unfoldr :: (b -> Maybe (a, b)) -> b -> JoinList a
unfoldr f = step where
  step st = case f st of
              Nothing -> Empty
              Just (a,st') -> a `cons` step st'


{-
para :: (a -> (JoinList a, b) -> b) -> b -> JoinList a -> b
para f b = step
  where step Empty      = b
        step (Single a) = f a (Empty,b) 
        step (t :++: u) = f (step t) (u, step u)
-}

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
   step (t :++: u) xs     = (t' :++: u',xs'') where
                              (t',xs')  = step t xs
                              (u',xs'') = step u xs'



