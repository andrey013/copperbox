{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Design
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

module Wumpus.Tree.Design
  (
    design
  , rotateAboutRoot
  )
  where

import Wumpus.Tree.Base

import Wumpus.Basic.Kernel              -- package: wumpus-basic

import Wumpus.Core                      -- package: wumpus-core

import Data.List 
import Data.Maybe
import Data.Tree




-- | XPos is an absolute position
--
type XPos u = u     

type XTree u a = Tree (XPos u, a)


-- | Delta - difference in X-positions.
--
type Delta u = u

-- A horizontal span.
--
data HSpan u = HSpan !u !u
  deriving (Eq,Ord,Show)



outsideMerge :: HSpan u -> HSpan u -> HSpan u
outsideMerge (HSpan p _) (HSpan _ q) = HSpan p q




moveSpan :: Num u => Delta u -> HSpan u -> HSpan u
moveSpan d (HSpan p q) = HSpan (p+d) (q+d)


newtype Extent u = Extent { span_list :: [HSpan u] }
  deriving (Eq,Show)


extlink :: u -> Extent u -> Extent u
extlink a (Extent as) = Extent $ (HSpan a a) :as 

-- note is this just for left ... ?
--
midtop :: Fractional u => u -> Extent u -> XPos u
midtop r (Extent [])        = r
midtop _ (Extent (HSpan p q:_)) = p + (0.5*(q-p))


-- merge \"moving right\"...
--
mergeMR :: Num u => Delta u -> Extent u -> Extent u -> Extent u
mergeMR dx (Extent xs) (Extent ys) = Extent $ step xs ys
  where
    step ps     []     = ps
    step []     qs     = map (moveSpan dx) qs
    step (p:ps) (q:qs) = outsideMerge p (moveSpan dx q) : step ps qs

-- dx is negative...
--
mergeML :: Num u => Delta u -> Extent u -> Extent u -> Extent u
mergeML dx (Extent xs) (Extent ys) = Extent $ step xs ys
  where
    step ps     []     = map (moveSpan dx) ps
    step []     qs     = qs
    step (p:ps) (q:qs) = outsideMerge (moveSpan dx p) q : step ps qs



extentZero :: Extent u
extentZero = Extent []

extentOne :: XPos u -> Extent u
extentOne x = Extent [HSpan x x]


-- 'moveTree' is now recursive...
--
moveTree :: Num u => Delta u -> XTree u a -> XTree u a
moveTree dx (Node (x,a) subtrees) = Node ((x+dx),a) subtrees'
  where
    subtrees' = map (moveTree dx) subtrees



fit :: (Fractional u, Ord u) 
    => Extent u -> Extent u -> u
fit a b = go (span_list a) (span_list b) 0.0 
  where
    go (HSpan _ p:ps) (HSpan q _:qs) acc = go ps qs (max acc (p - q + 1.0))
    go _          _                  acc = acc  


-- Fitting the children of a node...


fitleft :: (Fractional u, Ord u) 
        => [(XTree u a,Extent u)] -> ([XTree u a], Extent u)
fitleft []           = ([],extentZero)
fitleft ((l,ext):xs) = (l:ts,ext') -- left-most child unchanged
   where 
     (ext',ts)        = mapAccumL step ext xs

     step aex (t,ex)  = let dx = fit aex ex 
                        in (mergeMR dx aex ex, moveTree dx t)

fitright :: (Fractional u, Ord u) 
         => [(XTree u a, Extent u)] -> ([XTree u a], Extent u)
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

designl :: forall a u. (Fractional u, Ord u) 
        => Tree a -> (XTree u a, Extent u)
designl (Node a [])   = (Node (0.0,a)  [],    extentOne 0.0)
designl (Node a kids) = (Node (xpos,a) kids', ext1)
  where
    xs              :: [(XTree u a, Extent u)]
    xs              = map designl kids

    kids'           :: [XTree u a]
    ext0, ext1      :: Extent u
    (kids',ext0)    = fitleft xs

    xpos            = midtop 0.0 ext0
    ext1            = xpos `extlink` ext0


designr :: forall u a. (Fractional u, Ord u) 
        => XPos u -> Tree a -> (XTree u a, Extent u)
designr r (Node a [])   = (Node (r,a)  [],    extentOne r)
designr r (Node a kids) = (Node (xpos,a) kids', ext1)
  where
    xs              :: [(XTree u a, Extent u)]
    xs              = map (designr r) kids

    kids'           :: [XTree u a]
    ext0, ext1      :: Extent u
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
design :: (Fractional u, Ord u)
       => Point2 u -> ScalingContext u Int u -> Tree a -> CoordTree u a
design ro sctx t = rootOrientate ro $ scaleDesign sctx 0 t3
  where
    (t1,ext)                    = designl t
    (_, HSpan xmin xmax)        = stats ext
    width                       = xmax - xmin
    (t2,_)                      = designr width t
    
    -- reconcile the left and right drawings...
    t3                          = treeZipWith zfn t1 t2
    zfn (x0,a) (x1,_)           = (mean x0 x1,a)


-- Scale the tree. Originally the tree has no y-positions (but by 
-- recursion they can be counted) and x-positions are respective 
-- to the unit width 1.0.
--
scaleDesign :: Num uy 
            => ScalingContext ux uy u -> uy -> Tree (XPos ux, a) -> CoordTree u a
scaleDesign ctx lvl (Node (xpos,a) kids) = Node (pt,a) kids'
  where
    pt    = scalePt ctx xpos lvl
    kids' = map (scaleDesign ctx (lvl-1)) kids
     
    

rootOrientate :: Num u => Point2 u -> CoordTree u a -> CoordTree u a
rootOrientate (P2 ox oy) (Node (P2 x0 y0, val) kids) = 
    Node (P2 ox oy, val) $ map (mv (ox-x0) (oy-y0)) kids
  where
    mv dx dy (Node (P2 x y, a) ks) = let ks' = map (mv dx dy) ks 
                                     in Node (P2 (x+dx) (y+dy), a) ks'

-- Updating this to the latest Wumpus-Basic would make the 
-- function a query...
--
rotateAboutRoot :: (Real u, Floating u) 
                => Radian -> CoordTree u a -> CoordTree u a
rotateAboutRoot ang (Node (ogin,val) kids) =
    Node (ogin, val) $ map step kids
  where
    step (Node (p0, a) ks) = Node (rotA p0, a) $ map step ks

    rotA                   = rotateAbout ang ogin

-- find height and width
--
stats :: (Num u, Ord u) => Extent u -> (Int, HSpan u)
stats (Extent [])     = (0,HSpan 0 0)
stats (Extent (e:es)) = foldr fn (1,e) es
  where
    fn s1 (h, acc_span) = (h+1, minmaxMerge s1 acc_span)

mean :: Fractional u => u -> u -> u
mean x y = (x+y) / 2.0


minmaxMerge :: Ord u => HSpan u -> HSpan u -> HSpan u
minmaxMerge (HSpan p q) (HSpan p' q') = HSpan (min p p') (max q q')


treeZipWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
treeZipWith f (Node a xs) (Node b ys) = Node (f a b) (step xs ys)
  where
    step (p:ps) (q:qs) = treeZipWith f p q : step ps qs
    step _      _      = [] 




