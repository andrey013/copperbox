{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.View.Relation4
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A limited relation data type with a 4 element domain, this models 
-- the name table 
-- 
--------------------------------------------------------------------------------


module Graphics.OTFont.View.Relation4 where

import qualified Data.Foldable as F
import qualified Data.Set as Set

type Rel4 a b c d z = Set.Set ((a,b,c,d), z)

empty :: (Ord a, Ord b, Ord c, Ord d, Ord z) => Rel4 a b c d z
empty = Set.empty

insert :: (Ord a, Ord b, Ord c, Ord d, Ord z) =>
          a -> b -> c -> d -> z -> Rel4 a b c d z -> Rel4 a b c d z 
insert a b c d z = Set.insert ((a,b,c,d),z)

 
leftDom :: (a,b,c,d,z) -> ((a,b,c,d),z)
leftDom (a,b,c,d,z) = ((a,b,c,d),z)

unleftDom :: ((a,b,c,d),z) -> (a,b,c,d,z)
unleftDom ((a,b,c,d),z) = (a,b,c,d,z)


listToRel :: (Ord a, Ord b, Ord c, Ord d, Ord z) => 
             [(a,b,c,d,z)] -> Rel4 a b c d z
listToRel xs = Set.fromList $ map leftDom xs



relToList :: Rel4 a b c d z -> [(a,b,c,d,z)]
relToList r = map unleftDom $ Set.toList r

emptyRel :: Rel4 a b c d z
emptyRel = Set.empty


restrictDom :: (Ord a, Ord b, Ord c, Ord d, Ord z) => 
               (a -> b -> c -> d -> Bool) -> Rel4 a b c d z -> Rel4 a b c d z
restrictDom p r =  F.foldr f emptyRel r where
  f e@((a,b,c,d),_) s | p a b c d = Set.insert e s
                      | otherwise = s 


restrictDomA :: (Ord a, Ord b, Ord c, Ord d, Ord z) => 
                (a -> Bool) -> Rel4 a b c d z -> Rel4 a b c d z
restrictDomA p r =  F.foldr f emptyRel r where
  f e@((a,_,_,_),_) s | p a       = Set.insert e s
                      | otherwise = s 


restrictDomB :: (Ord a, Ord b, Ord c, Ord d, Ord z) => 
                (b -> Bool) -> Rel4 a b c d z -> Rel4 a b c d z
restrictDomB p r =  F.foldr f emptyRel r where
  f e@((_,b,_,_),_) s | p b       = Set.insert e s
                      | otherwise = s 
                      
restrictDomC :: (Ord a, Ord b, Ord c, Ord d, Ord z) => 
                (c -> Bool) -> Rel4 a b c d z -> Rel4 a b c d z
restrictDomC p r =  F.foldr f emptyRel r where
  f e@((_,_,c,_),_) s | p c       = Set.insert e s
                      | otherwise = s             
                      
restrictDomD :: (Ord a, Ord b, Ord c, Ord d, Ord z) => 
                (d -> Bool) -> Rel4 a b c d z -> Rel4 a b c d z
restrictDomD p r =  F.foldr f emptyRel r where
  f e@((_,_,_,d),_) s | p d       = Set.insert e s
                      | otherwise = s            

rangeL :: (Ord a, Ord b, Ord c, Ord d, Ord z) => 
          Rel4 a b c d z -> [z]
rangeL = F.foldr f [] where
    f e a = snd e : a
    
    