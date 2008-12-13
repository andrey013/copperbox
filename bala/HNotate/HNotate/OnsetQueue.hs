{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.OnsetQueue
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- A sequence with lists of concurrent events tagged by onset time.
--
--------------------------------------------------------------------------------


module HNotate.OnsetQueue (
    -- * Pending event queue
    OnsetQueue,

    -- * Operations
    buildQueue, {- , add, addP, upto, firstEvent, rest -}
    
    foldlOnsetQueue,
    ViewH(..), viewH,

    Show(..)
  ) where

import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Map as Map




newtype Elt a = Elt { getElts :: [a] }



newtype OnsetQueue idx a = OnsetQueue { getOnsetQueue :: Map.Map idx (Elt a) }


instance (Ord idx) => Monoid (OnsetQueue idx a) where
  mempty = mempty
  l `mappend` (OnsetQueue im) = foldl op l (Map.toAscList im)
    where
      op m (i,xs) = cat i (getElts xs) m

instance Show a => Show (Elt a) where
  showsPrec p (Elt xs) = showsPrec p xs

instance (Show idx, Show a) => Show (OnsetQueue idx a) where
  show (OnsetQueue im) = Map.showTree im


buildQueue :: (Ord idx, F.Foldable t) => 
              (a -> idx) -> (a -> b) -> t a -> OnsetQueue idx b
buildQueue f g t = F.foldl fn (OnsetQueue mempty) t
  where fn q ev = let (i,e) = (f ev, g ev) in add i e q
  
  
add :: Ord idx => idx -> evt -> OnsetQueue idx evt -> OnsetQueue idx evt
add i e (OnsetQueue q) = OnsetQueue $ fn (Map.lookup i q)
  where
    fn Nothing         = Map.insert i (Elt [e]) q
    fn (Just (Elt xs)) = Map.insert i (Elt $ e:xs) q

cat :: Ord idx => idx -> [evt] -> OnsetQueue idx evt -> OnsetQueue idx evt
cat i es (OnsetQueue q) = OnsetQueue $ fn (Map.lookup i q)
  where
    fn Nothing         = Map.insert i (Elt es) q
    fn (Just (Elt xs)) = Map.insert i (Elt $ xs ++ es) q



-- view of the head of the queue
data ViewH idx evt = EmptyQ | (idx,[evt]) :>> OnsetQueue idx evt

foldlOnsetQueue :: (a -> (idx,[b]) -> a) -> a -> OnsetQueue idx b -> a
foldlOnsetQueue fn a = foldl (adapt fn) a . Map.toAscList . getOnsetQueue
  where
    adapt :: (a -> (idx,[b]) -> a) -> (a -> (idx,Elt b) -> a) 
    adapt f = \acc (i,Elt e) -> f acc (i,e)

viewH :: OnsetQueue idx evt -> ViewH idx evt
viewH (OnsetQueue q) = case  Map.minViewWithKey q of
    Nothing              -> EmptyQ
    Just ((i,Elt xs),q') -> (i,xs) :>> OnsetQueue q'



