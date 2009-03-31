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

import Data.Sequence ( ViewL(..), viewl, ( |> ), ( <| ) )
import qualified Data.Sequence as S
import qualified Data.Foldable as F

foldS :: (b -> a -> b) -> b -> S.Seq a -> b
foldS = F.foldl'


-- LineTree is a tree that emphasizes its /horizontal linearity/ 
-- rather than a vertical decomposition through branching 
-- (c.f. bin tree and rose tree)

newtype LineTree e = LineTree { getLineTree :: S.Seq (LTNode e) }
    deriving (Show) 
    

data LTNode e = Single e
              | Overlay [LineTree e]  
    deriving (Show)  

instance Functor LineTree where
  fmap f (LineTree se) = LineTree (fmap (fmap f) se) 
      
instance Functor LTNode where
  fmap f (Single a)   = Single (f a) 
  fmap f (Overlay xs) = Overlay (fmap (fmap f) xs)

 
foldlLineTree :: (b -> a -> b) -> b -> LineTree a -> b
foldlLineTree f b (LineTree se) = foldS (foldlLTNode f) b se

foldlLTNode :: (b -> a -> b) -> b -> LTNode a -> b
foldlLTNode f b (Single a)   = f b a
foldlLTNode f b (Overlay xs) = F.foldl' (foldlLineTree f) b xs


stateFoldlS :: (st -> b -> a -> (b,st)) -> st -> b -> S.Seq a -> (b,st)
stateFoldlS f s b se = step b s (viewl se) where
    -- step :: (b,st) -> ViewL a -> (b,st)
    step b s EmptyL     = (b,s)
    step b s (a :< sa)  = let (b',s') = (f s b a) in 
                            (b',s') `seq` step b' s' (viewl sa)
     
stateFoldl :: (st -> b -> a -> (b,st)) -> st -> b -> [a] -> (b,st)
stateFoldl f s b xs = step b s xs where

    step b s []     = (b,s)
    step b s (a:as) = let (b',s') = (f s b a) in 
                          (b',s') `seq` step b' s' as
                              
nonpropFoldl :: (st -> b -> a -> (b,st)) -> st -> b -> [a] -> (b,st)
nonpropFoldl f s b xs = step b xs where

    step b []     = (b,s)
    step b (a:as) = let (b',_s) = (f s b a) in 
                        (b',_s) `seq` step b' as
                          
                          
stateFoldlLineTree :: (st -> b -> a -> (b,st)) -> st -> b -> LineTree a -> (b,st)
stateFoldlLineTree f s b (LineTree se) = stateFoldlS (stateFoldlLTNode f) s b se

stateFoldlLTNode :: (st -> b -> a -> (b,st)) -> st -> b -> LTNode a -> (b,st)
stateFoldlLTNode f s b (Single a)   = f s b a
stateFoldlLTNode f s b (Overlay xs) = nonpropFoldl (stateFoldlLineTree f) s b xs

flatten = foldlLineTree (|>) S.empty  

flattenSt = stateFoldlLineTree (\st se a -> (se |> (a,st), st+1)) 0 S.empty 


level' :: LineTree e -> [(S.Seq e)]
level' (LineTree sa) = step S.empty [] (viewl sa) 
  where
    step :: S.Seq e -> [(S.Seq e)] -> ViewL (LTNode e) -> [(S.Seq e)]
    step z zs EmptyL              = z:zs
  
    step z zs (Single e :< se)    = step (z |> e) zs (viewl se)
        
    step z zs (Overlay xs :< se)  = step z (zs++zs') (viewl se) where
                                      zs' = concat $ fmap level' xs  
      

level :: LineTree e -> [(S.Seq e)]
level = uncurry (flip (:)) . F.foldl' fn ([],S.empty) . getLineTree where
  fn (xss, se) (Single e)   = (xss, se |> e)
  fn (xss, se) (Overlay xs) = (xss++xss',se) where 
                                  xss' = concat $ fmap level xs

-- Note: state is 'forked' for Overlays, this is exactly the behaviour 
-- needed for identifying start times of overlays.
levelSt' :: (st -> e -> (z,st)) -> st -> LineTree e -> [(S.Seq z)] 
levelSt' f st0 = post . F.foldl' fn ([],S.empty,st0) . getLineTree where
  fn (xss,se,st) (Single e)   = (xss, se |> e', st') where (e',st') = f st e
  fn (xss,se,st) (Overlay xs) = (xss++xss',se,st) where     
                                  xss' = concat $ fmap (levelSt' f st) xs
  post (xss,se,_st)           = (se:xss)
    

-- Note: state is 'forked' for Overlays, this is exactly the behaviour 
-- needed for identifying start times of overlays.
-- alternative - just records start time for each overlay 
levelSt :: (st -> e -> st) -> st -> LineTree e -> [(st,S.Seq e)] 
levelSt upd st0 = post . F.foldl' fn ([],(st0,S.empty),st0) . getLineTree where

  fn (xss,(z,se),st) (Single e)   = (xss,       (z,se |> e), st') where 
                                        st' = upd st e
                                        
  fn (xss,(z,se),st) (Overlay xs) = (xss++xss', (z,se),      st) where     
                                        xss' = concat $ fmap (levelSt upd st) xs
                                        
  post (xss,se,_st)               = (se:xss)
    

    
lineTree :: LineTree a
lineTree = LineTree S.empty

(|*>) :: LineTree a -> LTNode a -> LineTree a
(|*>) (LineTree se) e = LineTree $ se |> e

poly              :: [LineTree a] -> LineTree a -> LineTree a
poly xs t         = t |*> Overlay xs

event             :: a -> LineTree a -> LineTree a
event e t         = t |*> Single e 




