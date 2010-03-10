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
  , OneManyL
    
  , one

  , fromList
  , toList 

  , accumMapL
  , isOne
  , isMany

  , intersperse
  , interinter                  -- not sure about this one ...   
  , gfoldOneManyL

  ) where


import Data.Semigroup           -- package: algebra

import Control.Applicative
import Data.Foldable hiding ( toList )
import qualified Data.List as List
import Data.Traversable

import Prelude hiding ( head, foldl, foldr )
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


newtype OneManyL a = OneManyL { getOneManyL :: [OneMany a] }
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- Instances


instance Functor OneMany where
  fmap f (One a)        = One $ f a
  fmap f (Many as)      = Many $ fmap f as

instance Functor OneManyL where
  fmap f = OneManyL . map (fmap f) . getOneManyL 



instance Foldable OneMany where
  foldMap f (One a)     = f a
  foldMap f (Many as)   = foldMap f as

  foldr f b (One a)     = f a b
  foldr f b (Many as)   = Pre.foldr f b as

  foldl f b (One a)     = f b a
  foldl f b (Many as)   = Pre.foldl f b as


instance Foldable OneManyL where
  foldMap f = foldMap (foldMap f) . getOneManyL

  foldr f b = List.foldr (flip (foldr f)) b . getOneManyL

  foldl f b = List.foldl (foldl f) b . getOneManyL


instance Traversable OneMany where
  traverse f (One a)    = One  <$> f a
  traverse f (Many as)  = Many <$> traverse f as


instance Traversable OneManyL where
  traverse f (OneManyL xs) = OneManyL <$> traverse (traverse f) xs


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


-- helper - I prefer accumMapL to mapAccumL 
-- (different arg placements)

accumMapList :: (x -> st -> (y,st)) -> [x] -> st -> ([y],st)
accumMapList f xs st0 = step xs st0 where
  step []     st = ([],st)
  step (a:as) st = (b:bs,st'') 
                   where (b,st')   = f a st
                         (bs,st'') = step as st' 
                                   
isMany :: OneMany a -> Bool
isMany (Many _) = True
isMany _        = False

isOne :: OneMany a -> Bool
isOne (One _)   = True
isOne _         = False



--------------------------------------------------------------------------------

intersperse :: a -> OneMany a -> OneMany a
intersperse _   (One x)   = One x
intersperse sep (Many xs) = Many (List.intersperse sep xs)


-- for Doc this might be better as a reduction...

interinter :: a -> a -> OneManyL a -> OneManyL a
interinter sepo sepi (OneManyL xs) = 
  OneManyL $ List.intersperse (one sepo) $ map (intersperse sepi) xs  

gfoldOneManyL :: (a -> b -> b) -> ([a] -> b -> b) -> b -> OneManyL a -> b
gfoldOneManyL f g b0 (OneManyL xs) = step b0 xs where
  step b []             = b
  step b (One a:rest)   = step (f a b)  rest
  step b (Many as:rest) = step (g as b) rest

--------------------------------------------------------------------------------
