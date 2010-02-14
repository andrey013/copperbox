{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.JoinList
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A \"join list\" datatype and operations. 
--
-- A join list is implemented a binary tree, so joining two 
-- lists (catenation, aka (++)) is a cheap operation. 
--
-- This constrasts with the regular list datatype which is a 
-- cons list: while consing on a regular list is by nature cheap, 
-- joining (++) is expensive. 
--
--------------------------------------------------------------------------------


module Data.JoinList 
  ( 
  -- * Join list datatype, opaque.
    JoinList

  -- * Views as per Data.Sequence  
  , ViewL(..)
  , ViewR(..)

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
  , head
  , last
  , tail
  , init
  , null
  , concat
  , length
  , map
  , reverse


  -- * Building join lists
  , replicate
  , repeated

  -- * Folds and unfolds
  , gfold
  , foldr
  , foldl
  , unfoldl
  , unfoldr

  -- * Views
  , viewl
  , viewr

  -- * Sublists
  , takeLeft
  , takeRight
  , dropLeft
  , dropRight 

  -- * Zipping (deprecated)
  , xzip
  , xzipWith

  
  ) where


import Control.Applicative hiding ( empty )

import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import Data.Monoid
import Data.Traversable ( Traversable(..) )

import Prelude hiding ( (++), concat, foldl, foldr, head, 
                        init, last, length,
                        map, null, replicate, reverse,
                        tail )


data JoinList a = Empty 
                | Single a 
                | JoinList a :++: JoinList a
  deriving (Eq)

data ViewL a = EmptyL | a :< (JoinList a)
  deriving (Eq,Show)

data ViewR a = EmptyR | (JoinList a) :> a
  deriving (Eq,Show)


--------------------------------------------------------------------------------

instance Show a => Show (JoinList a) where
  showsPrec _ xs = showString "fromList " . shows (toList xs) 

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

-- Views

instance Functor ViewL where
  fmap _ EmptyL         = EmptyL
  fmap f (a :< as)      = f a :< fmap f as

instance Functor ViewR where
  fmap _ EmptyR         = EmptyR
  fmap f (as :> a)      = fmap f as :> f a

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

-- | Extract the first element of a join list - i.e. the leftmost
-- element of the left spine. An error is thrown if the list is 
-- empty. 
-- 
-- This function performs a traversal down the left spine, so 
-- unlike @head@ on regular lists this function is not performed 
-- in constant time.
head :: JoinList a -> a
head Empty      = error "Data.JoinList.head: empty list"
head (Single a) = a
head (t :++: _) = head t



-- | Extract the last element of a join list - i.e. the rightmost
-- element of the right spine. An error is thrown if the list is 
-- empty.
last :: JoinList a -> a
last Empty      = error "Data.JoinList.head: empty list"
last (Single a) = a
last (_ :++: u) = last u

-- | Extract the elements after the head of a list. An error is thrown
-- if the list is empty.
tail :: JoinList a -> JoinList a
tail Empty             = error "Data.JoinList.tail: empty list"
tail (Single _)        = Empty
tail (Single _ :++: u) = u
tail (t        :++: u) = tail t :++: u


-- | Extract all the elements except the last one. An error is thrown
-- if the list is empty.
init :: JoinList a -> JoinList a
init Empty             = error "Data.JoinList.init: empty list"
init (Single _)        = Empty
init (t :++: Single _) = t
init (t :++: u)        = t :++: init u



-- | Test whether a join list is empty.
null :: JoinList a -> Bool
null Empty = True
null _     = False




-- | Concatenate a join list of join lists. 
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

reverse :: JoinList a -> JoinList a
reverse l = step l Empty where
  step Empty      acc = acc
  step (Single a) acc = acc `snoc` a
  step (t :++: u) acc = step t (step u acc)


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

--------------------------------------------------------------------------------
-- Views

-- | Access the left end of a sequence.
--
-- Unlike the corresponing operation on Data.Sequence this is 
-- not a cheap operation, the joinlist must be traversed down 
-- the left spine to find the leftmost node.
--
-- Also the traversal may involve changing the shape of the 
-- underlying binary tree.
--
viewl :: JoinList a -> ViewL a
viewl Empty      = EmptyL
viewl (Single a) = a :< Empty
viewl (t :++: u) = step t u where
   step Empty        r = viewl r
   step (Single a)   r = a :< r
   step (t' :++: u') r = step t' (u' :++: r)


-- | Access the rightt end of a sequence.
--
-- Unlike the corresponing operation on Data.Sequence this is 
-- not a cheap operation, the joinlist must be traversed down 
-- the right spine to find the rightmost node.
--
-- Also the traversal may involve changing the shape of the 
-- underlying binary tree.
--
viewr :: JoinList a -> ViewR a
viewr Empty      = EmptyR
viewr (Single a) = Empty :> a
viewr (t :++: u) = step t u where
   step l Empty        = viewr l
   step l (Single a)   = l :> a
   step l (t' :++: u') = step (l :++: t') u' 

--------------------------------------------------------------------------------
-- take etc

-- | Take the left @n@ elements of the list.
-- 
-- Implemented with 'viewl' hence the same performance caveats
-- apply.
--
takeLeft :: Int -> JoinList a -> JoinList a
takeLeft i _  | i <= 0 = Empty
takeLeft i xs          = case viewl xs of
                           EmptyL -> Empty
                           a :< t -> a `cons` takeLeft (i-1) t

-- | Take the right @n@ elements of the list.
-- 
-- Implemented with 'viewr' hence the same performance caveats
-- apply.
--
takeRight :: Int -> JoinList a -> JoinList a
takeRight i _  | i <= 0 = Empty
takeRight i xs          = case viewr xs of
                            EmptyR -> Empty 
                            t :> a -> takeRight (i-1) t `snoc` a 


-- | Drop the left @n@ elements of the list.
-- 
-- Implemented with 'viewl' hence the same performance caveats
-- apply.
--
dropLeft :: Int -> JoinList a -> JoinList a
dropLeft i xs  | i <= 0 = xs
dropLeft i xs           = case viewl xs of
                            EmptyL -> Empty
                            _ :< t -> dropLeft (i-1) t
                          


-- | Drop the right @n@ elements of the list.
-- 
-- Implemented with 'viewr' hence the same performance caveats
-- apply.
--
dropRight :: Int -> JoinList a -> JoinList a
dropRight i xs | i <= 0 = xs
dropRight i xs          = case viewr xs of
                            EmptyR -> Empty
                            t :> _ -> dropRight (i-1) t



--------------------------------------------------------------------------------
-- Zipping

-- | This function should be considered deprecated.
--
-- /cross zip/ - zip a join list against a regular list, 
-- maintaining the shape of the join list provided the lengths 
-- of the lists match.
--
xzip :: JoinList a -> [b] -> JoinList (a,b)
xzip = xzipWith (,)

-- | This function should be considered deprecated.
--
-- Generalized cross zip - c.f. zipWith on regular lists.
--
xzipWith :: (a -> b -> c) -> JoinList a -> [b] -> JoinList c
xzipWith fn xs0 ys0       = fst $ step xs0 ys0
  where 
   step Empty      xs     = (Empty,xs)
   step (Single a) (x:xs) = (Single (fn a x),xs)
   step (Single _) []     = (Empty,[])
   step (t :++: u) xs     = (t' :++: u',xs'') where
                              (t',xs')  = step t xs
                              (u',xs'') = step u xs'



