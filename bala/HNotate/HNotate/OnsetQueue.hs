
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
import qualified Data.IntMap as IM




newtype Elt a = Elt { getElts :: [a] }



newtype OnsetQueue a = OnsetQueue { getOnsetQueue :: IM.IntMap (Elt a) }

empty :: OnsetQueue a
empty = OnsetQueue $ IM.empty

instance Monoid (OnsetQueue a) where
  mempty = mempty
  l `mappend` (OnsetQueue im) = foldl op l (IM.toAscList im)
    where
      op m (i,xs) = cat i (getElts xs) m

instance Show a => Show (Elt a) where
  showsPrec p (Elt xs) = showsPrec p xs

instance Show a => Show (OnsetQueue a) where
  show (OnsetQueue im) = IM.showTree im


buildQueue :: (F.Foldable t) => (a -> Int) -> (a -> b) -> t a -> OnsetQueue b
buildQueue f g t = F.foldl fn (OnsetQueue mempty) t
  where fn q ev = let (i,e) = (f ev, g ev) in add i e q
  
  
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



-- view of the head of the queue
data ViewH evt = EmptyQ | (Int,[evt]) :>> OnsetQueue evt

foldlOnsetQueue :: (a -> (Int,[b]) -> a) -> a -> OnsetQueue b -> a
foldlOnsetQueue f a = foldl (adapt f) a . IM.toAscList . getOnsetQueue
  where
    adapt :: (a -> (Int,[b]) -> a) -> (a -> (Int,Elt b) -> a) 
    adapt f = \acc (i,Elt a) -> f acc (i,a)

viewH :: OnsetQueue evt -> ViewH evt
viewH (OnsetQueue q) = case  IM.minViewWithKey q of
    Nothing              -> EmptyQ
    Just ((i,Elt xs),q') -> (i,xs) :>> OnsetQueue q'



