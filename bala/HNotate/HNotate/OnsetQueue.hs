{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.OnsetQueue
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  mptc fundeps
--
-- A queue with elements tagged by onset time.
-- Principally for generating MIDI files when splitting \notes\
-- into @note-on@ and @note-off@. The note-off will occur at some
-- point in the future so it is queued to be realized later.
--
--------------------------------------------------------------------------------



module HNotate.OnsetQueue (
    -- * Pending event queue
    OnsetQueue,

    OnsetEvent(..),

    -- * Operations
    buildQueue, {- , add, addP, upto, firstEvent, rest -}

    ViewH(..), viewH,

    Show(..)
  ) where

import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.IntMap as IM




newtype Elt a = Elt { getElts :: [a] }

instance Functor Elt where
  fmap f (Elt xs) = Elt (fmap f xs)

instance F.Foldable Elt where
  foldMap f (Elt xs) = F.foldMap f xs


newtype OnsetQueue a = OnsetQueue { getOnsetQueue :: IM.IntMap (Elt a) }

empty :: OnsetQueue a
empty = OnsetQueue $ IM.empty

instance Monoid (OnsetQueue a) where
  mempty = mempty
  l `mappend` (OnsetQueue im) = foldl op l (IM.toAscList im)
    where
      op m (i,xs) = cat i (getElts xs) m


instance Functor OnsetQueue where
  fmap f (OnsetQueue q) = OnsetQueue (fmap (fmap f) q)

instance F.Foldable OnsetQueue where
  foldMap f (OnsetQueue a)           = F.foldMap (F.foldMap f) a

instance Show a => Show (Elt a) where
  showsPrec p (Elt xs) = showsPrec p xs

instance Show a => Show (OnsetQueue a) where
  show (OnsetQueue im) = IM.showTree im

class OnsetEvent a b | a -> b  where
  onset :: a -> (Int,b)

add :: Int -> evt -> OnsetQueue evt -> OnsetQueue evt
add i e (OnsetQueue q) = OnsetQueue $ fn (IM.lookup i q)
  where
    fn Nothing         = IM.insert i (Elt [e]) q
    fn (Just (Elt xs)) = IM.insert i (Elt $ e:xs) q

cat :: Int -> [evt] -> OnsetQueue evt -> OnsetQueue evt
cat i es (OnsetQueue q) = OnsetQueue $ fn (IM.lookup i q)
  where
    fn Nothing         = IM.insert i (Elt es) q
    fn (Just (Elt xs)) = IM.insert i (Elt $ xs ++ es) q


buildQueue :: (OnsetEvent a b, F.Foldable t) => t a -> OnsetQueue b
buildQueue t = F.foldl fn (OnsetQueue mempty) t
  where fn q ev = let (i,e) = onset ev in add i e q

-- view of the head of the queue
data ViewH evt = EmptyQ | (Int,[evt]) :>> OnsetQueue evt


instance Functor ViewH where
  fmap f EmptyQ               = EmptyQ
  fmap f ((i,es) :>> r)       = ((i, fmap f es) :>> fmap f r)

instance F.Foldable ViewH where
  foldMap f EmptyQ            = mempty
  foldMap f ((i,es) :>> r)    = (F.foldMap f es) `mappend` F.foldMap f r



viewH :: OnsetQueue evt -> ViewH evt
viewH (OnsetQueue q) = case  IM.minViewWithKey q of
    Nothing              -> EmptyQ
    Just ((i,Elt xs),q') -> (i,xs) :>> OnsetQueue q'



