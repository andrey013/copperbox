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
  , start
  , end
  , Element(..)
  , linearfold
 
  ) where

import Neume.Core.Utils.FunctorN
import Neume.Core.Utils.StateMap

-- package: joinlist
import Data.JoinList      hiding ( empty, cons, map )
import qualified Data.JoinList as JL

import Control.Applicative hiding ( empty )
import Data.Foldable
import Data.Monoid
import Data.Traversable

data TStream tag leaf =  TStream { getTStream :: JoinList (Node tag leaf) }
  deriving (Eq,Show)

data Node tag leaf = Elem     leaf
                   | StartTag tag
                   | EndTag
  deriving (Eq,Show)


class PrintTag t where
  startShowS :: t -> ShowS
  endShowS   :: t -> ShowS


nil :: TStream tag leaf
nil = TStream JL.empty

cons :: leaf -> TStream tag leaf -> TStream tag leaf
cons a = TStream . JL.cons (Elem a) . getTStream 

start :: tag -> TStream tag leaf -> TStream tag leaf
start a = TStream . JL.cons (StartTag a) . getTStream

end   :: TStream tag leaf -> TStream tag leaf
end   = TStream . JL.cons EndTag . getTStream



data Element tag leaf = Start tag
                      | Leaf  leaf
                      | End   tag
  deriving (Eq,Show)

linearfold :: (b -> Element tag leaf -> b) -> b -> TStream tag leaf -> b
linearfold f b0 = step [] b0 . viewl . getTStream where
  step _       b EmptyL             = b
  step stk     b (Elem a     :< rs) = step stk     (f b $ Leaf a)  (viewl rs)
  step stk     b (StartTag t :< rs) = step (t:stk) (f b $ Start t) (viewl rs)
  step (t:stk) b (EndTag     :< rs) = step stk     (f b $ End t)   (viewl rs) 
  step []      _ _                  = error "linearfold - malformed tree"



--------------------------------------------------------------------------------
-- Functor instance

-- Functor does not touch tag... (needs bifunctor)

instance Functor (TStream tag) where
  fmap f (TStream xs) = TStream $ fmap (fmap f) xs

instance Functor (Node tag) where
  fmap _ (StartTag t) = StartTag t
  fmap f (Elem a)     = Elem (f a)
  fmap _ EndTag       = EndTag

instance Foldable (TStream tag) where
  foldMap f (TStream xs) = foldMap (foldMap f) xs 

instance Foldable (Node tag) where
  foldMap _ (StartTag _) = mempty
  foldMap f (Elem a)     = f a
  foldMap _ EndTag       = mempty



instance Traversable (TStream tag) where
  traverse f (TStream xs)    = TStream <$> traverse (traverse f) xs

instance Traversable (Node tag) where
  traverse f (Elem a)        = Elem <$> f a
  traverse _ (StartTag t)    = pure  $  StartTag t 
  traverse _ EndTag          = pure  $  EndTag

-- Fmap2

instance FMap2 TStream where
  fmap2 f g (TStream xs)    = TStream $ fmap (fmap2 f g) xs

instance FMap2 Node where
  fmap2 _ g (Elem a)        = Elem $ g a
  fmap2 f _ (StartTag t)    = StartTag $ f t
  fmap2 _ _ EndTag          = EndTag 






-- Note stmap does not account for TEnd so it is not suitable
-- for traversals that need the tree shape

instance StateMap2 TStream where
  stmap2 f g st (TStream xs)  = (TStream xs', st')
                                where
                                  (xs',st') = stmap (stmap2 f g) st xs


instance StateMap2 Node where
  stmap2 _ g st (Elem a)      = (Elem a', st') 
                                where
                                  (a',st')     = g st a

  stmap2 f _ st (StartTag t)  = (StartTag t', st') 
                                where
                                  (t',st')     = f st t

  stmap2 _ _ st EndTag        = (EndTag, st) 
