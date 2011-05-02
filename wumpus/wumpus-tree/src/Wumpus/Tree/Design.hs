{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Design
-- Copyright   :  (c) Stephen Tetley 2010-2011
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

module Wumpus.Tree.Design
  (

  -- * Design a tree
    UW
  , CoordTree
  , design

  -- * Post transform a tree design
  , scaleTree
  , orientateTree

  )
  where

import Wumpus.Tree.Base

import Wumpus.Basic.Kernel              -- package: wumpus-basic

import Wumpus.Core                      -- package: wumpus-core

import Data.List 
import Data.Maybe
import Data.Tree


-- | Tree unit width.
-- 
-- Trees are designed with 1.0 as the ideal width between nodes.
-- This is represented as a specific newtype so it can be 
-- contextually scaled after the design, before the tree is drawn.
--
newtype UW = UW { getUW :: Double }
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show UW where
  showsPrec p d = showsPrec p (getUW d)

instance InterpretUnit UW where
  normalize _ = realToFrac
  dinterp   _ = realToFrac




-- | Tree annotated with positions.
-- 
-- This is the result of 'design'.
--
type CoordTree a = Tree (Point2 UW, a)


-- | XPos is an absolute position
--
type XPos = UW

type XTree a = Tree (XPos, a)


-- | Delta - difference in X-positions.
--
type Delta = UW

-- A horizontal span.
--
data HSpan = HSpan !UW !UW
  deriving (Eq,Ord,Show)



outsideMerge :: HSpan -> HSpan -> HSpan
outsideMerge (HSpan p _) (HSpan _ q) = HSpan p q




moveSpan :: Delta -> HSpan -> HSpan
moveSpan d (HSpan p q) = HSpan (p+d) (q+d)


newtype Extent = Extent { span_list :: [HSpan] }
  deriving (Eq,Show)


extlink :: UW -> Extent -> Extent
extlink a (Extent as) = Extent $ (HSpan a a) :as 

-- note is this just for left ... ?
--
midtop :: UW -> Extent -> XPos
midtop r (Extent [])        = r
midtop _ (Extent (HSpan p q:_)) = p + (0.5*(q-p))


-- merge \"moving right\"...
--
mergeMR :: Delta -> Extent -> Extent -> Extent
mergeMR dx (Extent xs) (Extent ys) = Extent $ step xs ys
  where
    step ps     []     = ps
    step []     qs     = map (moveSpan dx) qs
    step (p:ps) (q:qs) = outsideMerge p (moveSpan dx q) : step ps qs

-- dx is negative...
--
mergeML :: Delta-> Extent -> Extent -> Extent
mergeML dx (Extent xs) (Extent ys) = Extent $ step xs ys
  where
    step ps     []     = map (moveSpan dx) ps
    step []     qs     = qs
    step (p:ps) (q:qs) = outsideMerge (moveSpan dx p) q : step ps qs



extentZero :: Extent
extentZero = Extent []

extentOne :: XPos -> Extent
extentOne x = Extent [HSpan x x]


-- 'moveTree' is now recursive...
--
moveTree :: Delta -> XTree a -> XTree a
moveTree dx (Node (x,a) subtrees) = Node ((x+dx),a) subtrees'
  where
    subtrees' = map (moveTree dx) subtrees



fit :: Extent -> Extent -> UW
fit a b = go (span_list a) (span_list b) 0.0 
  where
    go (HSpan _ p:ps) (HSpan q _:qs) acc = go ps qs (max acc (p - q + 1.0))
    go _          _                  acc = acc  


-- Fitting the children of a node...


fitleft :: [(XTree a,Extent)] -> ([XTree a], Extent)
fitleft []           = ([],extentZero)
fitleft ((l,ext):xs) = (l:ts,ext') -- left-most child unchanged
   where 
     (ext',ts)        = mapAccumL step ext xs

     step aex (t,ex)  = let dx = fit aex ex 
                        in (mergeMR dx aex ex, moveTree dx t)

fitright :: [(XTree a, Extent)] -> ([XTree a], Extent)
fitright = post . foldr fn Nothing
  where
    post                        = fromMaybe ([],extentZero)
    fn (t,ex) Nothing           = Just ([t],ex)
    fn (t,ex) (Just (ts,aex))   = Just (t':ts,aex')
       where
         dx     = negate $ fit ex aex
         t'     = moveTree dx t
         aex'   = mergeML dx ex aex




-- | Design the tree from the left.
-- 
-- Left and right tree designs are merged.
--
designl :: forall a. Tree a -> (XTree a, Extent)
designl (Node a [])   = (Node (0.0,a)  [],    extentOne 0.0)
designl (Node a kids) = (Node (xpos,a) kids', ext1)
  where
    xs              :: [(XTree a, Extent)]
    xs              = map designl kids

    kids'           :: [XTree a]
    ext0, ext1      :: Extent
    (kids',ext0)    = fitleft xs

    xpos            = midtop 0.0 ext0
    ext1            = xpos `extlink` ext0

-- | Design the tree from the right.
-- 
-- Right and left tree designs are merged.
--
designr :: forall a. XPos -> Tree a -> (XTree a, Extent)
designr r (Node a [])   = (Node (r,a)  [],    extentOne r)
designr r (Node a kids) = (Node (xpos,a) kids', ext1)
  where
    xs              :: [(XTree a, Extent)]
    xs              = map (designr r) kids

    kids'           :: [XTree a]
    ext0, ext1      :: Extent
    (kids',ext0)    = fitright xs

    xpos            = midtop r ext0
    ext1            = xpos `extlink` ext0


-- | Design a tree, properly balancing the child nodes oriented 
-- at root. 
--
-- As the design has no y-positions (but by recursion they can be 
-- counted) and x-positions are respective to the unit distance 
-- 1.0 separating nodes it is rescaled as a post-processing step
-- into drawable coordinates. 
--
design :: Tree a -> CoordTree a
design t = rootOrientate zeroPt $ decorateYPosns 0 t3
  where
    (t1,ext)                    = designl t
    (_, HSpan xmin xmax)        = stats ext
    width                       = xmax - xmin
    (t2,_)                      = designr width t
    
    -- reconcile the left and right drawings...
    t3                          = treeZipWith zfn t1 t2
    zfn (x0,a) (x1,_)           = (mean x0 x1,a)


-- Originally the tree has no y-positions, recurse through the 
-- tree adding them...
--
decorateYPosns :: UW -> Tree (XPos, a) -> CoordTree a
decorateYPosns lvl (Node (xpos,a) kids) = Node (pt,a) kids'
  where
    pt    = P2 xpos lvl
    kids' = map (decorateYPosns (lvl-1)) kids
     
    

rootOrientate :: Point2 UW -> CoordTree a -> CoordTree a
rootOrientate (P2 ox oy) (Node (P2 x0 y0, val) kids) = 
    Node (P2 ox oy, val) $ map (mv (ox-x0) (oy-y0)) kids
  where
    mv dx dy (Node (P2 x y, a) ks) = let ks' = map (mv dx dy) ks 
                                     in Node (P2 (x+dx) (y+dy), a) ks'


-- find height and width
--
stats :: Extent -> (Int, HSpan)
stats (Extent [])     = (0,HSpan 0 0)
stats (Extent (e:es)) = foldr fn (1,e) es
  where
    fn s1 (h, acc_span) = (h+1, minmaxMerge s1 acc_span)

mean :: Fractional u => u -> u -> u
mean x y = (x+y) / 2.0


minmaxMerge :: HSpan -> HSpan -> HSpan
minmaxMerge (HSpan p q) (HSpan p' q') = HSpan (min p p') (max q q')


treeZipWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
treeZipWith f (Node a xs) (Node b ys) = Node (f a b) (step xs ys)
  where
    step (p:ps) (q:qs) = treeZipWith f p q : step ps qs
    step _      _      = [] 





--------------------------------------------------------------------------------
-- Post design transformations...


-- | 'scaleTree' : @ sibling_distance * level_distance * CoordTree -> Tree @
--
-- Scale a CoordTree - this forms a tree where the node label
-- is a pair of @Point2 u@ and an @a@ (usually a LocImage).
-- 
scaleTree :: InterpretUnit u 
          => u -> u -> CoordTree a -> Query (Tree (Point2 u, a))
scaleTree sib_dist lvl_dist tree = 
    getFontSize >>= \sz -> 
    let fn = mkFun sz in return $ fmap (bimapL fn) tree
  where
    mkFun sz = \(P2 x y) -> let ux = sib_dist * (dinterp sz $ realToFrac x)
                                uy = lvl_dist * (dinterp sz $ realToFrac y)
                            in P2 ux uy




-- | Orientate the Tree according to it\'s drawing direction.
-- 
-- This is a rotation about the root node.
--
orientateTree :: (Real u, Floating u)
              => TreeDirection -> Tree (Point2 u, a) -> Tree (Point2 u, a)
orientateTree TREE_DOWN  tree = tree
orientateTree TREE_UP    tree = rotateAboutRoot pi tree
orientateTree TREE_LEFT  tree = rotateAboutRoot (1.5*pi) tree
orientateTree TREE_RIGHT tree = rotateAboutRoot (0.5*pi) tree


rotateAboutRoot :: (Real u, Floating u)
                => Radian -> Tree (Point2 u, a) -> Tree (Point2 u, a)
rotateAboutRoot ang (Node (ogin,val) kids) =
    Node (ogin, val) $ map step kids
  where
    step (Node (v0, a) ks) = Node (rotA v0, a) $ map step ks

    rotA                   = rotateAbout ang ogin




