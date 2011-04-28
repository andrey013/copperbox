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

    radialConn
  , familyConn

  ) where

import Wumpus.Tree.Base

import Wumpus.Drawing.Paths.Absolute            -- package: wumpus-drawing

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


-- | 'radialConn' has no need for the TreeDirection or height step.
-- 
radialConn :: ( Real u, Floating u, InterpretUnit u
              , CenterAnchor a, RadialAnchor a 
              , u ~ DUnit a) 
           => OTMAnchorConn u a
radialConn _ _ a xs = mconcat $ map fn $ radialNodes a xs
  where
    fn (p0,p1) = vertexPP [p0,p1] >>= dcOpenPath




--------------------------------------------------------------------------------
-- 

-- Note - can the \"crossbar\" of a famillyConn cannot be 
-- calcuated parent-to-arbitrary-child?
-- 
-- Probably we need to know half the height step, rather than 
-- calculate it from anchors.
--

-- Drawing a fmaily connector is quite horrible...



familyConn :: ( Real u, Floating u, Ord u, Tolerance u, InterpretUnit u
              , CenterAnchor a, CardinalAnchor a 
              , u ~ DUnit a ) 
           => OTMAnchorConn u a
familyConn _   _ _ [] = mempty
familyConn dir h a xs = 
    let hh        = 0.5 * h
        (paF,caF) = famAnchors dir
        ptick     = outtick hh (center a) (paF a)
        cticks    = map (\o -> outtick hh (center o) (caF o)) xs
        kids      = imgconcat ptick cticks
    in graphic_ ptick `oplus` (graphic_ $ elaborateR0 kids fn)
  where
    fn ps = case linkAll ps of
              Nothing -> emptyLocGraphic `at` (center a)
              Just path -> toPrimPath path >>= dcOpenPath


imgconcat :: Image u a -> [Image u a] -> Image u [a] 
imgconcat alt []      = alt >>= \(Ans o1 x) -> return $ Ans o1 [x]
imgconcat _   (gf:gs) = step gf gs 
  where
    step ma []     = ma >>= \(Ans o1 x) -> return $ Ans o1 [x]
    step ma (k:ks) = ma >>= \(Ans o1 x) -> 
                     step k ks >>= \(Ans o2 xs) ->
                     return $ Ans (o1 `oplus` o2) (x:xs)


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
    in pushR0 (replaceAns p2) (straightLine p1 p2)

-- | The input list is expected to be ordered...
--
linkAll :: (Real u, Floating u, Ord u, Tolerance u) 
        => [Point2 u] -> Maybe (AbsPath u)
linkAll [] = Nothing
linkAll xs = Just $ optimizeLines $ vertexPath xs






--------------------------------------------------------------------------------


