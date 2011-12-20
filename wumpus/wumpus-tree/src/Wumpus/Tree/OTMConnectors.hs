{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.Draw
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- One-to-many connectors.
--
--------------------------------------------------------------------------------

module Wumpus.Tree.OTMConnectors
  (

    radialOTM
  , blankOTM
  , familyOTM
  , splayOTM

  ) where

import Wumpus.Tree.Base

import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Data.Maybe
import Data.Monoid
import Prelude hiding ( lookup )



radialOTM :: ( Real u, Floating u, InterpretUnit u
             , RadialAnchor node, CenterAnchor node, u ~ DUnit node) 
          => OTMAnchorConn node u 
radialOTM _ _ a as = mconcat $ map fn $ radialNodes a as
  where
    fn (s,t)      = connect straightConnector s t



-- Pulling out the points (nodes) from parent plus list of 
-- children.


radialNodes :: ( Real u, Floating u, InterpretUnit u
               , CenterAnchor a, RadialAnchor a
               , u ~ DUnit a )  
            => a -> [a] -> [(Point2 u, Point2 u)]
radialNodes a as = map fn as
  where
    actr = center a
    fn x = (radialAnchor ang0 a , radialAnchor ang1 x) 
             where (ang0, ang1) = anchorAngles actr (center x)
        

anchorAngles :: (Real u, Floating u) 
             => Point2 u -> Point2 u -> (Radian,Radian)
anchorAngles f t = (theta0, theta1)
  where
    conn_v  = pvec f t
    theta0  = vdirection conn_v
    theta1  = if theta0 < pi then theta0 + pi else theta0 - pi




blankOTM :: OTMAnchorConn node u
blankOTM _ _ _ _ = mempty



-- 
-- @radialConn@ cannot be represented as a connector from
-- one-point-to-many-points as the initial points all start from
-- slightly different places. 
--

--------------------------------------------------------------------------------
-- 

-- Note - can the \"crossbar\" of a famillyConn cannot be 
-- calculated parent-to-arbitrary-child?
-- 
-- Probably we need to know half the height step 
-- (vcenter-to-vcenter), rather than calculate it from anchors.
--




-- Drawing a family connector is quite horrible, we need to know
-- both the tree direction and the vertical height between layers.



familyOTM :: ( Real u, Floating u, Ord u, Tolerance u, InterpretUnit u
             , CenterAnchor node, CardinalAnchor node
             , u ~ DUnit node ) 
          => OTMAnchorConn node u
familyOTM _   _ _ [] = mempty
familyOTM dir h a xs = 
    let hh        = 0.5 * h
        (paF,caF) = famAnchors dir
        ptick     = outtick hh (center a) (paF a)
        cticks    = map (\o -> outtick hh (center o) (caF o)) xs
        kids      = sequence cticks
    in ignoreAns ptick `mappend` (ignoreAns $ elaborateAbove kids fn)
  where
    fn ps = case linkAll ps of
              Nothing -> emptyLocImage `at` (center a)
              Just path -> renderPath_ OSTROKE path



famAnchors :: (CardinalAnchor a, u ~ DUnit a ) 
           => TreeDirection -> (a -> Anchor u, a -> Anchor u)
famAnchors TREE_UP    = (north, south)
famAnchors TREE_DOWN  = (south, north)
famAnchors TREE_LEFT  = (west,  east)
famAnchors TREE_RIGHT = (east,  west)



outtick :: (Real u, Floating u, InterpretUnit u) 
        => u -> Point2 u -> Point2 u -> Image u (Point2 u)
outtick ll p0 p1 = 
    let v0  = pvec p0 p1
        ang = vdirection v0
        v1  = avec ang ll
        p2  = p0 .+^ v1
    in replaceAns p2 (straightLine p1 p2)

-- | The input list is expected to be ordered...
--
linkAll :: (Real u, Floating u, Ord u, Tolerance u) 
        => [Point2 u] -> Maybe (AbsPath u)
linkAll [] = Nothing
linkAll xs = Just $ optimizeLines $ vertexPath xs





splayOTM :: ( Real u, Floating u, Ord u, Tolerance u, InterpretUnit u
            , CenterAnchor node, CardinalAnchor node 
            , u ~ DUnit node) 
          => OTMAnchorConn node u
splayOTM _   _ _ [] = mempty
splayOTM dir _ a xs = let (paF,caF) = famAnchors dir
                          p0        = paF a
                      in mconcat $ map (\x -> fn p0 (caF x)) xs
  where
    fn p0 p1 = straightLine p0 p1




--------------------------------------------------------------------------------


