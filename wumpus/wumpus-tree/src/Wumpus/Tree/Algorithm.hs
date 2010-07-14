{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Algorithm
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Diamond
-- 
--------------------------------------------------------------------------------

module Wumpus.Tree.Algorithm
  ( 
    PNode
  , design
  , LocNode
  , scaleTree
  
  ) where

import Wumpus.Core ( Point2(..) )       -- package: wumpus-core
import Wumpus.Basic.Utils.HList         -- package: wumpus-basic

import Data.Monoid
import Data.Tree

data SP u = SP !u !u
  deriving (Eq,Ord,Show)


longZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
longZipWith f = step where
  step xs     []     = xs
  step []     ys     = ys
  step (x:xs) (y:ys) = f x y : step xs ys

unzipMap :: (a -> (b,c)) -> [a] -> ([b],[c])
unzipMap f = step 
  where
    step []     = ([],[])
    step (a:as) = (b:bs,c:cs) where (b,c)   = f a 
                                    (bs,cs) = step as
                  

newtype Extent = Extent { getExtent :: [SP Double] }
  deriving (Eq,Show)

extmap :: (SP Double -> SP Double) -> Extent -> Extent
extmap f = Extent . map f . getExtent

extcons :: SP Double -> Extent -> Extent
extcons a (Extent as) = Extent $ a:as

instance Monoid Extent where
  mempty   = Extent []
  Extent xs `mappend` Extent ys  = Extent $ longZipWith op xs ys
                                   where op (SP p _) (SP _ q) = SP p q


type PNode a = (Double,a)

moveTree :: Tree (PNode a) -> Double -> Tree (PNode a)
moveTree (Node (x,a) subtrees) dx = Node ((x+dx),a) subtrees


moveExtent :: Extent -> Double -> Extent
moveExtent e x = extmap (\(SP p q) -> SP (p+x) (q+x)) e


fit :: Extent -> Extent -> Double
fit a b = step (getExtent a) (getExtent b) 0.0 
  where
    step (SP _ p:ps) (SP q _:qs) acc = step ps qs (max acc (p - q + 1.0))
    step _           _           acc = acc  



fitlistl :: [Extent] -> [Double]
fitlistl xs = step xs mempty
  where
    step []     _   = []
    step (e:es) acc = x : step es (acc `mappend` moveExtent e x)
                      where x = fit acc e


-- Using a Hughes list with snoc can save one reverse...
--
fitlistr :: [Extent] -> [Double]
fitlistr xs = toListH $ step (reverse xs) mempty
  where
    step []     _   = emptyH
    step (e:es) acc = (step es (moveExtent e x `mappend` acc)) `snocH` x
                      where x = negate $ fit e acc 

mean :: Double -> Double -> Double
mean x y = (x+y) / 2.0

fitlist :: [Extent] -> [Double]
fitlist es = zipWith mean (fitlistl es) (fitlistr es)


design :: Tree a -> Tree (PNode a)
design = fst . design'

design' :: forall a. Tree a -> (Tree (PNode a), Extent)
design' (Node val subtrees) = (resultTree, resultExtent)
  where
    (trees, extents) = unzipMap design' subtrees
    
    positions        :: [Double]
    positions        = fitlist extents

    ptrees           :: [Tree (PNode a)]
    ptrees           = zipWith moveTree trees positions

    pextents         :: [Extent]
    pextents         = zipWith moveExtent extents positions

    resultExtent     :: Extent
    resultExtent     = (SP 0.0 0.0) `extcons` mconcat pextents

    resultTree       :: Tree (PNode a)
    resultTree       = Node (0.0,val) ptrees


type LocNode u a = (Point2 u, a)


-- Because Wumpus used bottom-left as the origin, the root needs
-- the largest Y-value...
-- 
-- The first traversal, labels nodes with their depth and makes 
-- the x-coord absolute. The second traversal inverts the depth 
-- and applies the scaling functions. 
--
scaleTree :: (Double -> u, Int -> u) -> Tree (PNode a) -> Tree (LocNode u a)
scaleTree (fx,fy) tree = fmap step2 tree1
  where
    (height,tree1)               = step1 0 0 tree
    step1 ix lvl (Node (x,a) xs) = 
        let (ns,kids) = unzipMap (step1 (ix+x) (lvl+1)) xs
        in (maxima ns, Node ((ix+x,lvl),a) kids) 

    step2 ((x,lvl),a)         = (P2 (fx x) (fy $ height - lvl), a)

maxima :: (Num a, Ord a) => [a] -> a
maxima [] = 0
maxima xs = maximum xs 