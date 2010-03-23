{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.TaggedStream
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- A tagged stream - a linear representation of tree
-- (strictly speaking forests - list of trees) where tree
-- nodes are encoded with start and end tags in a flat stream.  
--
--------------------------------------------------------------------------------

module Neume.Core.Utils.TaggedStream
  (
    TStream(..)
  , PrintTag(..)
  , nil 
  , cons
  , Element(..)
  , linearfold
 
  ) where

import Neume.Core.Utils.FunctorN
import Neume.Core.Utils.StateMap

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable



data TStream tag leaf = TNil 
                      | TLeaf  leaf (TStream tag leaf)
                      | TStart tag  (TStream tag leaf)
                      | TEnd        (TStream tag leaf)
  deriving (Eq,Show)


class PrintTag t where
  startShowS :: t -> ShowS
  endShowS   :: t -> ShowS


nil :: TStream tag leaf
nil = TNil

cons :: leaf -> TStream tag leaf -> TStream tag leaf
cons a = TLeaf a 


data Element tag leaf = Start tag
                      | Leaf  leaf
                      | End   tag
  deriving (Eq,Show)

linearfold :: (b -> Element tag leaf -> b) -> b -> TStream tag leaf -> b
linearfold f = step [] where
  step _       b TNil          = b
  step stk     b (TLeaf a rs)  = step stk (f b $ Leaf a) rs
  step stk     b (TStart t rs) = step (t:stk) (f b $ Start t) rs
  step (t:stk) b (TEnd rs)     = step stk (f b $ End t) rs 
  step []      _ _             = error "linearfold - malformed tree"


--------------------------------------------------------------------------------
-- Functor instance

instance Functor (TStream tag) where
  fmap _ TNil            = TNil
  fmap f (TLeaf a rest)  = TLeaf (f a) $ fmap f rest
  fmap f (TStart t rest) = TStart t $ fmap f rest
  fmap f (TEnd rest)     = TEnd $ fmap f rest


-- Note, Foldable oblivious to tags...

instance Foldable (TStream tag) where
  foldMap _ TNil            = mempty
  foldMap f (TLeaf a rest)  = f a `mappend` foldMap f rest
  foldMap f (TStart _ rest) = foldMap f rest
  foldMap f (TEnd rest)     = foldMap f rest


  foldr f b0 = step b0 where
    step b TNil             = b
    step b (TLeaf a rest)   = f a (step b rest)
    step b (TStart _ rest)  = step b rest
    step b (TEnd rest)      = step b rest

  foldl f b0 = step b0 where
    step b TNil             = b
    step b (TLeaf a rest)   = step (f b a) rest
    step b (TStart _ rest)  = step b rest
    step b (TEnd rest)      = step b rest



instance Traversable (TStream tag) where
  traverse _ TNil            = pure TNil
  traverse f (TLeaf a rest)  = TLeaf    <$> f a <*> traverse f rest
  traverse f (TStart t rest) = TStart t <$> traverse f rest
  traverse f (TEnd rest)     = TEnd     <$> traverse f rest


-- Fmap2

instance FMap2 TStream where
  fmap2 _ _ TNil            = TNil
  fmap2 f g (TLeaf a rest)  = TLeaf (g a) $ fmap2 f g rest
  fmap2 f g (TStart t rest) = TStart (f t) $ fmap2 f g rest
  fmap2 f g (TEnd rest)     = TEnd $ fmap2 f g rest


-- Note stmap does not account for TEnd so it is not suitable
-- for traversals that need the tree shape

instance StateMap2 TStream where
  stmap2 _ _ st TNil            = (TNil,st)
  stmap2 f g st (TLeaf a rest)  = (TLeaf a' rest', st'') 
                                  where
                                    (a',st')     = g st a
                                    (rest',st'') = stmap2 f g st' rest

  stmap2 f g st (TStart t rest) = (TStart t' rest', st'') 
                                  where
                                    (t',st')     = f st t
                                    (rest',st'') = stmap2 f g st' rest

  stmap2 f g st (TEnd rest)     = (TEnd rest', st') 
                                  where
                                    (rest',st') = stmap2 f g st rest
