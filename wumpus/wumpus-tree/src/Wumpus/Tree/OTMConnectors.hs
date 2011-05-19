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

    radialOTMC
  , blankOTMC
  , familyOTMC
  , splayOTMC

  ) where

import Wumpus.Tree.Base

-- import Wumpus.Drawing.Basis.DrawingPrimitives   -- package: wumpus-drawing
import Wumpus.Drawing.Paths.Absolute

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Data.Monoid





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


-- 
-- @radialConn@ cannot be represented as a connector from
-- one-point-to-many-points as the initial points all start from
-- slightly different places. 
--


-- | 'radialOTMC' has no need for the TreeDirection or height step.
-- 
radialOTMC :: ( Real u, Floating u, InterpretUnit u
              , CenterAnchor a, RadialAnchor a 
              , u ~ DUnit a) 
           => OTMAnchorConn u a
radialOTMC _ _ a xs = mconcat $ map fn $ radialNodes a xs
  where
    fn (p0,p1) = straightLine p0 p1

-- | Blank connector - nothing is drawn.
--
blankOTMC :: ( Real u, Floating u, InterpretUnit u
            , CenterAnchor a
            , u ~ DUnit a) 
         => OTMAnchorConn u a
blankOTMC _ _ a _ = emptyLocImage `at` center a


--------------------------------------------------------------------------------
-- 

-- Note - can the \"crossbar\" of a famillyConn cannot be 
-- calcuated parent-to-arbitrary-child?
-- 
-- Probably we need to know half the height step, rather than 
-- calculate it from anchors.
--

-- Drawing a fmaily connector is quite horrible...



familyOTMC :: ( Real u, Floating u, Ord u, Tolerance u, InterpretUnit u
              , CenterAnchor a, CardinalAnchor a 
              , u ~ DUnit a ) 
           => OTMAnchorConn u a
familyOTMC _   _ _ [] = mempty
familyOTMC dir h a xs = 
    let hh        = 0.5 * h
        (paF,caF) = famAnchors dir
        ptick     = outtick hh (center a) (paF a)
        cticks    = map (\o -> outtick hh (center o) (caF o)) xs
        kids      = sequence cticks
    in ignoreAns ptick `mappend` (ignoreAns $ selaborate kids fn)
  where
    fn ps = case linkAll ps of
              Nothing -> emptyLocImage `at` (center a)
              Just path -> ignoreAns $ drawOpenPath path



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





splayOTMC :: ( Real u, Floating u, Ord u, Tolerance u, InterpretUnit u
              , CenterAnchor a, CardinalAnchor a 
              , u ~ DUnit a ) 
           => OTMAnchorConn u a
splayOTMC _   _ _ [] = mempty
splayOTMC dir _ a xs = 
    let (paF,caF) = famAnchors dir
        p0        = paF a
    in mconcat $ map (\x -> fn p0 (caF x)) xs
  where
    fn p0 p1 = straightLine p0 p1



--------------------------------------------------------------------------------


