{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.LineTree
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- A data type for trees emphasizing horizontal decomposition through
-- appending, rather vertical decomposition through branching. 
--
--------------------------------------------------------------------------------



module HNotate.LineTree where


import Data.Sequence ( ( |> ) )
import qualified Data.Sequence as S
import qualified Data.Foldable as F




-- LineTree is a tree that emphasizes its /horizontal linearity/ 
-- rather than a vertical decomposition through branching 
-- (c.f. bin tree and rose tree)

newtype LineTree e = LineTree { getLineTree :: S.Seq (LTNode e) }
    deriving (Show) 
    

-- Unfortunately, LTNode in not representable by Cardinal as an Overlay is
-- made of LineTrees and not the element type @e@.

data LTNode e = LTNode e
              | Overlay [LineTree e]  
    deriving (Show)  

instance Functor LineTree where
  fmap f (LineTree se) = LineTree (fmap (fmap f) se) 

      
instance Functor LTNode where
  fmap f (LTNode a)   = LTNode (f a) 
  fmap f (Overlay xs) = Overlay (fmap (fmap f) xs)

 
foldlLineTree :: (b -> a -> b) -> b -> LineTree a -> b
foldlLineTree f b (LineTree se) = foldS (foldlLTNode f) b se

foldlLTNode :: (b -> a -> b) -> b -> LTNode a -> b
foldlLTNode f b (LTNode a)   = f b a
foldlLTNode f b (Overlay xs) = F.foldl' (foldlLineTree f) b xs


foldS :: (b -> a -> b) -> b -> S.Seq a -> b
foldS = F.foldl'

  

-- Note: state is 'forked' for Overlays, this is exactly the behaviour 
-- needed for identifying start times of overlays.
-- alternative - just records start time for each overlay 
levelSt :: (st -> e -> st) -> st -> LineTree e -> [(st,S.Seq e)] 
levelSt upd st0 = post . F.foldl' fn ([],(st0,S.empty),st0) . getLineTree where

  fn (xss,(z,se),st) (LTNode e)   = (xss,       (z,se |> e), st') where 
                                        st' = upd st e
                                        
  fn (xss,(z,se),st) (Overlay xs) = (xss++xss', (z,se),      st) where     
                                        xss' = concat $ fmap (levelSt upd st) xs
                                        
  post (xss,se,_st)               = (se:xss)
    

    
lineTree :: LineTree a
lineTree = LineTree S.empty

(|*>) :: LineTree a -> LTNode a -> LineTree a
(|*>) (LineTree se) e = LineTree $ se |> e

overlays          :: [LineTree a] -> LineTree a -> LineTree a
overlays xs t     = t |*> Overlay xs

event             :: a -> LineTree a -> LineTree a
event e t         = t |*> LTNode e 




