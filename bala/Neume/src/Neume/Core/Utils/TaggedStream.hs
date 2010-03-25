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
    TaggedStream(..)
  , TagShowS(..)
  , ViewNext(..)
  , nil 
  , cons
  , start
  , end
  , Element(..)
  , linearfold
  , viewNext
  , forest
 
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

data TaggedStream t a = TaggedStream { 
        getTaggedStream :: JoinList (Node t a) 
      }
  deriving (Eq,Show)

data Node t a = Elem     a
              | StartTag t
              | EndTag
  deriving (Eq,Show)


class TagShowS t where
  startS :: t -> ShowS
  endS   :: t -> ShowS


nil :: TaggedStream t a
nil = TaggedStream JL.empty

cons :: a -> TaggedStream t a -> TaggedStream t a
cons a = tcons (Elem a)


tcons :: Node t a -> TaggedStream t a -> TaggedStream t a
tcons a (TaggedStream xs) = TaggedStream $ a `JL.cons` xs

tsnoc :: TaggedStream t a -> Node t a -> TaggedStream t a
tsnoc (TaggedStream xs) a = TaggedStream $ xs `JL.snoc` a


start :: t -> TaggedStream t a -> TaggedStream t a
start a = TaggedStream . JL.cons (StartTag a) . getTaggedStream

end   :: TaggedStream t a -> TaggedStream t a
end   = TaggedStream . JL.cons EndTag . getTaggedStream



data Element t a = Start t
                 | Leaf  a
                 | End   t
  deriving (Eq,Show)

linearfold :: (b -> Element t a -> b) -> b -> TaggedStream t a -> b
linearfold f b0 = step [] b0 . viewl . getTaggedStream where
  step _       b EmptyL             = b
  step stk     b (Elem a     :< rs) = step stk     (f b $ Leaf a)  (viewl rs)
  step stk     b (StartTag t :< rs) = step (t:stk) (f b $ Start t) (viewl rs)
  step (t:stk) b (EndTag     :< rs) = step stk     (f b $ End t)   (viewl rs) 
  step []      _ _                  = error "linearfold - malformed tree"

data ViewNext t a = EmptyStream
                  | Elementary a (TaggedStream t a)
                  | Level      t (TaggedStream t a) (TaggedStream t a)
  deriving (Eq,Show)

viewNext :: TaggedStream t a -> ViewNext t a
viewNext = outer . viewl . getTaggedStream where
  outer EmptyL              = EmptyStream
  outer (Elem a     :< rs)  = Elementary a (TaggedStream rs)
  outer (StartTag t :< rs)  = inner t (0::Int) JL.empty (viewl rs)
  outer (EndTag     :< _ )  = error "viewNext - malformed stream, unexpected end tag" 

  inner t 0 jl (EndTag :< rs) = Level t (TaggedStream jl) (TaggedStream rs)
  inner _ _ _  EmptyL         = error "viewNext - malformed stream, no end tag"
  inner t n jl (a      :< rs) = case a of
    StartTag _ -> inner t (n+1) (jl `JL.snoc` a) (viewl rs)
    Elem _     -> inner t n     (jl `JL.snoc` a) (viewl rs)
    EndTag     -> inner t (n-1) (jl `JL.snoc` a) (viewl rs)

forest :: TaggedStream t a -> [Either a (TaggedStream t a)]
forest = step . viewNext where
  step EmptyStream       = []
  step (Elementary a rs) = Left a : step (viewNext rs)
  step (Level t lvl rs)  = Right lvl' : step (viewNext rs)
                           where lvl' = start t (lvl `tsnoc` EndTag)

--------------------------------------------------------------------------------
-- Functor instance

-- Functor does not touch tag... (needs bifunctor)

instance Functor (TaggedStream t) where
  fmap f (TaggedStream xs) = TaggedStream $ fmap (fmap f) xs

instance Functor (Node t) where
  fmap _ (StartTag t) = StartTag t
  fmap f (Elem a)     = Elem (f a)
  fmap _ EndTag       = EndTag

instance Foldable (TaggedStream t) where
  foldMap f (TaggedStream xs) = foldMap (foldMap f) xs 

instance Foldable (Node t) where
  foldMap _ (StartTag _) = mempty
  foldMap f (Elem a)     = f a
  foldMap _ EndTag       = mempty



instance Traversable (TaggedStream t) where
  traverse f (TaggedStream xs)    = TaggedStream <$> traverse (traverse f) xs

instance Traversable (Node t) where
  traverse f (Elem a)        = Elem <$> f a
  traverse _ (StartTag t)    = pure  $  StartTag t 
  traverse _ EndTag          = pure  $  EndTag

-- Fmap2

instance FMap2 TaggedStream where
  fmap2 f g (TaggedStream xs)    = TaggedStream $ fmap (fmap2 f g) xs

instance FMap2 Node where
  fmap2 _ g (Elem a)        = Elem $ g a
  fmap2 f _ (StartTag t)    = StartTag $ f t
  fmap2 _ _ EndTag          = EndTag 






-- Note stmap does not account for TEnd so it is not suitable
-- for traversals that need the tree shape

instance StateMap2 TaggedStream where
  stmap2 f g st (TaggedStream xs)  = (TaggedStream xs', st')
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
