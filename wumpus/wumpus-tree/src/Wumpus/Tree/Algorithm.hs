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
-- A variant of the tree drawing algorithm from 
-- Andrew Kennedy - Functional Pearls Drawing Trees 1996.
--
-- Acknowledgment - although based on Andrew Kennedy\'s algorithm,
-- this version uses absolute extents rather than relative ones 
-- and is a somewhat different in detail if not in spirit to the 
-- original.
--
-- Any mistakes are mine of course.
-- 
--------------------------------------------------------------------------------

module Wumpus.Tree.Algorithm
  (
    CoordTree
  , design
  )
  where

import Wumpus.Core ( Point2(..) )       -- package: wumpus-core

import Data.List 
import Data.Maybe
import Data.Tree



-- | XPos is an absolute position
--
type XPos = Double      

type XTree a = Tree (XPos,a)


-- | Delta - difference in X-positions.
--
type Delta = Double

data Span = S !XPos !XPos
  deriving (Eq,Ord,Show)

type CoordTree u a = Tree (Point2 u, a)


outsideMerge :: Span -> Span -> Span
outsideMerge (S p _) (S _ q) = S p q

moveSpan :: Delta -> Span -> Span
moveSpan d (S p q) = S (p+d) (q+d)


newtype Extent = Extent { span_list :: [Span] }
  deriving (Eq,Show)


extlink :: XPos -> Extent -> Extent
extlink a (Extent as) = Extent (S a a:as)

-- note is this just for left ... ?
midtop :: XPos -> Extent -> XPos 
midtop r (Extent [])        = r
midtop _ (Extent (S p q:_)) = p + (0.5*(q-p))


-- merge \"moving right\"...
mergeMR :: Delta -> Extent -> Extent -> Extent
mergeMR dx (Extent xs) (Extent ys) = Extent $ step xs ys
  where
    step ps     []     = ps
    step []     qs     = map (moveSpan dx) qs
    step (p:ps) (q:qs) = outsideMerge p (moveSpan dx q) : step ps qs

-- dx is negative...
--
mergeML :: Delta -> Extent -> Extent -> Extent
mergeML dx (Extent xs) (Extent ys) = Extent $ step xs ys
  where
    step ps     []     = map (moveSpan dx) ps
    step []     qs     = qs
    step (p:ps) (q:qs) = outsideMerge (moveSpan dx p) q : step ps qs



extentZero :: Extent
extentZero = Extent []

extentOne :: XPos -> Extent
extentOne x = Extent [S x x]


-- 'moveTree' is now recursive...
--
moveTree :: Delta -> XTree a -> XTree a
moveTree dx (Node (x,a) subtrees) = Node ((x+dx),a) subtrees'
  where
    subtrees' = map (moveTree dx) subtrees



fit :: Extent -> Extent -> Double
fit a b = step (span_list a) (span_list b) 0.0 
  where
    step (S _ p:ps) (S q _:qs) acc = step ps qs (max acc (p - q + 1.0))
    step _          _          acc = acc  


-- Fitting the children of a node...


fitleft :: [(XTree a,Extent)] -> ([XTree a], Extent)
fitleft []           = ([],extentZero)
fitleft ((l,ext):xs) = (l:ts,ext') -- left-most child unchanged
   where 
     (ext',ts)        = mapAccumL step ext xs

     step aex (t,ex)  = let dx = fit aex ex 
                        in (mergeMR dx aex ex, moveTree dx t)

fitright :: [(XTree a,Extent)] -> ([XTree a], Extent)
fitright = post . foldr fn Nothing
  where
    post                        = fromMaybe ([],extentZero)
    fn (t,ex) Nothing           = Just ([t],ex)
    fn (t,ex) (Just (ts,aex))   = Just (t':ts,aex')
       where
         dx     = negate $ fit ex aex
         t'     = moveTree dx t
         aex'   = mergeML dx ex aex



-- Note - this will tell how wide the tree is...
-- though the last exten is not necessarily the widest.

designl :: forall a. Tree a -> (XTree a, Extent)
designl (Node a [])   = (Node (0.0,a)  [],    extentOne 0.0)
designl (Node a kids) = (Node (xpos,a) kids', ext1)
  where
    xs              :: [(XTree a,Extent)]
    xs              = map designl kids

    kids'           :: [XTree a]
    ext0, ext1      :: Extent
    (kids',ext0)    = fitleft xs

    xpos            = midtop 0.0 ext0
    ext1            = xpos `extlink` ext0


designr :: forall a. XPos -> Tree a -> (XTree a, Extent)
designr r (Node a [])   = (Node (r,a)  [],    extentOne r)
designr r (Node a kids) = (Node (xpos,a) kids', ext1)
  where
    xs              :: [(XTree a,Extent)]
    xs              = map (designr r) kids

    kids'           :: [XTree a]
    ext0, ext1      :: Extent
    (kids',ext0)    = fitright xs

    xpos            = midtop r ext0
    ext1            = xpos `extlink` ext0


design :: (Double -> u, Int -> u) -> Tree a -> CoordTree u a
design (fx,fy) t = label 0 t3
  where
    (t1,ext)                    = designl t
    (h,S xmin xmax)             = stats ext
    width                       = xmax - xmin
    (t2,_)                      = designr width t
    
    -- reconcile the left and right drawings...
    t3                          = treeZipWith zfn t1 t2
    
    mkPt x lvl                  = P2 (fx x) (fy $ h - lvl)
    label lvl (Node (x,a) kids) = Node (mkPt x lvl, a) kids'
       where
         kids' = map (label (lvl+1)) kids 

    zfn (x0,a) (x1,_)           = (mean x0 x1,a)

-- find height and width
--
stats :: Extent -> (Int,Span)
stats (Extent [])     = (0,S 0 0)
stats (Extent (e:es)) = foldr fn (1,e) es
  where
    fn (S x0 x1) (h, S xmin xmax) = (h+1, S (min x0 xmin) (max x1 xmax))

mean :: Double -> Double -> Double
mean x y = (x+y) / 2.0


treeZipWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
treeZipWith f (Node a xs) (Node b ys) = Node (f a b) (step xs ys)
  where
    step (p:ps) (q:qs) = treeZipWith f p q : step ps qs
    step _      _      = [] 




