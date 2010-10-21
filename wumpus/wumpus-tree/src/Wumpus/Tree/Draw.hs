{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Draw
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Drawing the tree using Wumpus-Basic.
--
--------------------------------------------------------------------------------

module Wumpus.Tree.Draw 
  (
    drawTree
  , drawFamilyTree

  ) where

import Wumpus.Tree.Base

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Anchors                     -- package: wumpus-basic
import Wumpus.Basic.Dots.AnchorDots
import Wumpus.Basic.Graphic   

import Data.VectorSpace                         -- package: vector-space

import Control.Monad
import Data.Tree hiding ( drawTree )



---------------------------------------------------------------------------------
-- Draw individual connector between parent and each child node.

drawTree :: (a -> TreeNode) 
         -> DrawingContext 
         -> CoordTree Double a 
         -> HPrim Double
drawTree drawF ctx tree = execDrawing ctx $ drawTop drawF tree 


drawTop :: (a -> TreeNode) -> CoordTree Double a -> Drawing Double ()
drawTop fn (Node (pt,a) ns) = do 
    ancr <- drawi $ fn a `at` pt
    mapM_ (draw1 fn ancr) ns

draw1 :: (a -> TreeNode) 
      -> DotAnchor Double 
      -> CoordTree Double a 
      -> Drawing Double ()
draw1 fn ancr_from (Node (pt,a) ns) = do
    ancr <- drawi $ fn a `at` pt
    draw $ connector ancr_from ancr
    mapM_ (draw1 fn ancr) ns   


connector :: (Floating u, Real u, InnerSpace (Vec2  u)) 
          => DotAnchor u -> DotAnchor u -> Graphic u
connector a1 a2 = openStroke $ vertexPath [p1,p2]
  where  
    (ang0,ang1)    = anchorAngles (center a1) (center a2)
    p1             = radialAnchor ang0 a1
    p2             = radialAnchor ang1 a2 




anchorAngles :: (Floating u, Real u, InnerSpace (Vec2  u)) 
             => Point2 u -> Point2 u -> (Radian,Radian)
anchorAngles f t = (theta0, theta1)
  where
    conn_v  = pvec f t
    theta0  = direction conn_v
    theta1  = if theta0 < pi then theta0 + pi else theta0 - pi
    

--------------------------------------------------------------------------------
-- Draw in /family tree/ style

drawFamilyTree :: (a -> TreeNode) 
               -> DrawingContext 
               -> CoordTree Double a 
               -> HPrim Double
drawFamilyTree drawF ctx tree = execDrawing ctx $ drawFamily drawF tree 


drawFamily :: (a -> TreeNode)  
           -> CoordTree Double a 
           -> Drawing Double (DotAnchor Double)
drawFamily fn (Node (pt,a) ns) = do
    ancr <- drawi $ fn a `at` pt
    xs   <- mapM (drawFamily fn) ns   
    when (not $ null xs) $ draw $ famconn (south ancr) (map north xs)
    return ancr

famconn :: (Fractional u, Ord u) => Point2 u -> [Point2 u] -> Graphic u
famconn _       []         = error "famconn - empty list"
famconn pt_from [p1]       = famconn1 pt_from p1
famconn pt_from xs@(p1:_)  = oconcat downtick (horizontal : upticks)
   where
     hh         = halfHeight pt_from p1
     downtick   = straightLine (vvec (-hh)) pt_from
     horizontal = midline (vdisplace (-hh) pt_from) xs 
     upticks    = map (straightLine (vvec hh)) xs

midline :: (Fractional u, Ord u) => Point2 u -> [Point2 u] -> Graphic u
midline _        []           = error "midline - empty list" 
midline (P2 _ y) (P2 x0 _:zs) = 
    let (a,b) = foldr fn (x0,x0) zs in straightLineBetween (P2 a y) (P2 b y)
  where   
    fn (P2 x _) (lo,hi) | x < lo    = (x,hi)
                        | x > hi    = (lo,x)
                        | otherwise = (lo,hi)

halfHeight :: Fractional u => Point2 u -> Point2 u -> u
halfHeight (P2 _ ya) (P2 _ yb) = 0.5 * (abs $ ya - yb)
 
-- special case - should always be a vertical, but...
famconn1 :: Fractional u => Point2 u -> Point2 u -> Graphic u
famconn1 a@(P2 xa _) b@(P2 xb _) 
    | xa == xb  = straightLineBetween a b
    | otherwise = openStroke $ vertexPath [a,m1,m2,b] 
  where
    hh = halfHeight a b
    m1 = vecdisplace (vvec (-hh)) a  
    m2 = vecdisplace (hvec $ xb - xa) m1
