{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.OneMany
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data type for one or many.
--
-- Structurally the same as OneList - but used for a different 
-- purpose.
--
--------------------------------------------------------------------------------

module Neume.Core.Utils.OneMany
  (
    OneMany
    
  , one

  , fromList
  , toList 

  , accumMapL
  , isOne
  , isMany
   
  ) where


import Data.Semigroup           -- package: algebra

import Control.Applicative
import Data.Foldable hiding ( toList )
import Data.Traversable

import Prelude hiding ( head )
import qualified Prelude as Pre

-- For convienence I use a list for Many, but don't expose the 
-- constructors so bad values (e.g. Many []) cannot be created.
-- 
-- We could have defined this type - which structurally enforces
-- that Many must have (>1) elements:
--
-- > data OneMany a = One a | Many a (OneMany a) 
-- 
-- However using that would require many conversions.
-- Note that it is identical to OneList.
-- 

data OneMany a = One a | Many [a]
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- Instances

instance Functor OneMany where
  fmap f (One a)        = One $ f a
  fmap f (Many as)      = Many $ fmap f as

instance Foldable OneMany where
  foldMap f (One a)     = f a
  foldMap f (Many as)   = foldMap f as

  foldr f b (One a)     = f a b
  foldr f b (Many as)   = Pre.foldr f b as

  foldl f b (One a)     = f b a
  foldl f b (Many as)   = Pre.foldl f b as


instance Traversable OneMany where
  traverse f (One a)    = One  <$> f a
  traverse f (Many as)  = Many <$> traverse f as


instance Semigroup (OneMany e) where
  (One a)   `append` (One b)    = Many [a,b]
  (Many as) `append` (One b)    = Many (as ++ [b])
  (One a)   `append` (Many bs)  = Many (a:bs)
  (Many as) `append` (Many bs)  = Many (as++bs)
 
--------------------------------------------------------------------------------
-- | Construct One.
one :: a -> OneMany a
one = One



-- | Construct Many. Not this function throws a error if the list has
-- zero or one elements
fromList :: [a] -> OneMany a
fromList []     = error "OneMany.fromList: cannot build Many from empty list"
fromList [a]    = One a
fromList as     = Many as


toList :: OneMany a -> [a]
toList (One x)    = [x]
toList (Many xs)  = xs


accumMapL :: (x -> st -> (y,st)) -> OneMany x -> st -> (OneMany y,st)
accumMapL f (One x)   st = let (y,st') = f x st in (One y,st')
accumMapL f (Many xs) st = (Many ys,st')
                             where (ys,st') = accumMapList f xs st
                                   
isMany :: OneMany a -> Bool
isMany (Many _) = True
isMany _        = False

isOne :: OneMany a -> Bool
isOne (One _)   = True
isOne _         = False



--------------------------------------------------------------------------------
-- helper - I prefer accumMapL to mapAccumL 
-- (different arg placements)

accumMapList :: (x -> st -> (y,st)) -> [x] -> st -> ([y],st)
accumMapList f xs st0 = step xs st0 where
  step []     st = ([],st)
  step (a:as) st = (b:bs,st'') 
                   where (b,st') = f a st
                         (bs,st'') = step as st' 