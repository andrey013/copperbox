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
import Wumpus.Tree.TreeBuildMonad

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Dots.AnchorDots

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Tree hiding ( drawTree )




drawTree :: (Real u, Floating u, InterpretUnit u) 
          => NodeAnnoRefs u -> CoordTree u (TreeNodeAns u) -> TreeDrawing u
drawTree annos tree = drawStep annos radialConns tree >> return ()



drawFamilyTree :: (Real u, Floating u, InterpretUnit u) 
          => NodeAnnoRefs u -> CoordTree u (TreeNodeAns u) -> TreeDrawing u
drawFamilyTree annos tree = drawStep annos familyConn tree >> return ()


drawStep :: (Real u, Floating u) 
         => NodeAnnoRefs u 
         -> (DotAnchor u -> [DotAnchor u] -> Graphic u)
         -> CoordTree u (TreeNodeAns u) -> TraceDrawing u (DotAnchor u)
drawStep annos connF (Node (pt,(fn, mb_ix)) ns) = do 
    ancr <- drawi $ fn `at` pt
    xs   <- mapM (drawStep annos connF) ns   
    when (not $ null xs) $ draw $ connF ancr xs
    drawAnno annos ancr mb_ix
    return ancr

drawAnno :: NodeAnnoRefs u -> DotAnchor u -> Maybe Int -> TraceDrawing u ()
drawAnno _    _    Nothing   = return ()
drawAnno refs ancr (Just ix) = maybe (return ()) sk $ IntMap.lookup ix refs
  where
    sk fn = draw $ fn ancr



radialConns :: ( Real u, Floating u, InterpretUnit u
               , CenterAnchor t u, RadialAnchor t u ) 
            => t u -> [t u] -> Graphic u
radialConns a []     = center a >>= \p0 -> emptyLocGraphic `at` p0
radialConns a (x:xs) = oconcat (connector a x) (map (connector a) xs)



connector :: ( Real u, Floating u, InterpretUnit u
             , CenterAnchor t u, RadialAnchor t u )  
          => t u -> t u -> Graphic u
connector a0 a1 = 
   center a0 >>= \p0 -> 
   center a1 >>= \p1 ->
   let (ang0, ang1) = anchorAngles p0 p1 
   in radialAnchor ang0 a0 >>= \pt0 -> 
      radialAnchor ang1 a1 >>= \pt1 -> vertexPP [pt0,pt1] >>= openStroke




anchorAngles :: (Real u, Floating u) 
             => Point2 u -> Point2 u -> (Radian,Radian)
anchorAngles f t = (theta0, theta1)
  where
    conn_v  = pvec f t
    theta0  = vdirection conn_v
    theta1  = if theta0 < pi then theta0 + pi else theta0 - pi
    





--------------------------------------------------------------------------------
-- 

familyConn :: ( Real u, Fractional u, InterpretUnit u
              , CenterAnchor t u, CardinalAnchor t u ) 
           => t u -> [t u] -> Graphic u
familyConn a [] = center a >>= \p0 -> emptyLocGraphic `at` p0
familyConn a xs = south a >>= \p0 -> mapM north xs >>= \ps -> famconn p0 ps

famconn :: (Fractional u, Ord u, InterpretUnit u) 
        => Point2 u -> [Point2 u] -> Graphic u
famconn _       []         = error "famconn - empty list"
famconn pt_from [p1]       = famconn1 pt_from p1
famconn pt_from xs@(p1:_)  = oconcat downtick (horizontal : upticks)
   where
     hh         = halfHeight pt_from p1
     downtick   = locStraightLine (vvec (-hh)) `at` pt_from
     horizontal = midline (displaceV (-hh) pt_from) xs 
     upticks    = map (locStraightLine (vvec hh) `at`) xs

midline :: (Fractional u, Ord u, InterpretUnit u) 
        => Point2 u -> [Point2 u] -> Graphic u
midline _        []           = error "midline - empty list" 
midline (P2 _ y) (P2 x0 _:zs) = 
    let (a,b) = foldr fn (x0,x0) zs in straightLine (P2 a y) (P2 b y)
  where   
    fn (P2 x _) (lo,hi) | x < lo    = (x,hi)
                        | x > hi    = (lo,x)
                        | otherwise = (lo,hi)

halfHeight :: Fractional u => Point2 u -> Point2 u -> u
halfHeight (P2 _ ya) (P2 _ yb) = 0.5 * (abs $ ya - yb)
 
-- special case - should always be a vertical, but...
--
famconn1 :: (Fractional u, InterpretUnit u)
         => Point2 u -> Point2 u -> Graphic u
famconn1 a@(P2 xa _) b@(P2 xb _) 
    | xa == xb  = straightLine a b
    | otherwise = vertexPP [a,m1,m2,b] >>= openStroke
  where
    hh = halfHeight a b
    m1 = displaceV (-hh)     a  
    m2 = displaceH (xb - xa) m1
