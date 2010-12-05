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
  )
  where

import Wumpus.Tree.Base

import Wumpus.Basic.Kernel              -- package: wumpus-basic

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

data Span u = S !(XPos u) !(XPos u)
  deriving (Eq,Ord,Show)



outsideMerge :: Span u -> Span u -> Span u
outsideMerge (S p _) (S _ q) = S p q

moveSpan :: Num u => Delta u -> Span u -> Span u
moveSpan d (S p q) = S (p+d) (q+d)


newtype Extent u = Extent { span_list :: [Span u] }
  deriving (Eq,Show)


extlink :: XPos u -> Extent u -> Extent u
extlink a (Extent as) = Extent (S a a:as)

-- note is this just for left ... ?
--
midtop :: Fractional u => XPos u -> Extent u -> XPos u
midtop r (Extent [])        = r
midtop _ (Extent (S p q:_)) = p + (0.5*(q-p))


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
extentOne x = Extent [S x x]


-- 'moveTree' is now recursive...
--
moveTree :: Num u => Delta u -> XTree u a -> XTree u a
moveTree dx (Node (x,a) subtrees) = Node ((x+dx),a) subtrees'
  where
    subtrees' = map (moveTree dx) subtrees



fit :: (Fractional u, Ord u) 
    => Extent u -> Extent u -> u
fit a b = step (span_list a) (span_list b) 0.0 
  where
    step (S _ p:ps) (S q _:qs) acc = step ps qs (max acc (p - q + 1.0))
    step _          _          acc = acc  


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


design :: (Fractional u, Ord u)
       => ScalingContext u Int u -> Tree a -> CoordTree u a
design sctx t = runScaling sctx (label 0 t3)
  where
    (t1,ext)                    = designl t
    (h,S xmin xmax)             = stats ext
    width                       = xmax - xmin
    (t2,_)                      = designr width t
    
    -- reconcile the left and right drawings...
    t3                          = treeZipWith zfn t1 t2
    
    mkPt x lvl                  = scalePt x (h - lvl)
    label lvl (Node (x,a) kids) = do pt <- mkPt x lvl
                                     kids' <- mapM (label (lvl+1)) kids
                                     return $ Node (pt,a) kids'

    zfn (x0,a) (x1,_)           = (mean x0 x1,a)



-- find height and width
--
stats :: (Num u, Ord u) => Extent u -> (Int, Span u)
stats (Extent [])     = (0,S 0 0)
stats (Extent (e:es)) = foldr fn (1,e) es
  where
    fn (S x0 x1) (h, S xmin xmax) = (h+1, S (min x0 xmin) (max x1 xmax))

mean :: Fractional u => u -> u -> u
mean x y = (x+y) / 2.0


treeZipWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
treeZipWith f (Node a xs) (Node b ys) = Node (f a b) (step xs ys)
  where
    step (p:ps) (q:qs) = treeZipWith f p q : step ps qs
    step _      _      = [] 




