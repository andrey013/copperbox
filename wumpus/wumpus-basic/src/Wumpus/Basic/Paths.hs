{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Paths
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Extended path type - more amenable for complex drawings than
-- the type in Wumpus-Core.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Paths 
  where

import Wumpus.Basic.Paths.Datatypes

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Sequence ( singleton ) 

-- It would be good not to have a name-clash with Wumpus-Core



path1c :: (Floating u, Ord u, InnerSpace (Vec2 u)) => Curve u -> BPath u
path1c c = BPath len (singleton $ BCurveSeg len c)
  where
    len = curveLength c

-- | Curve-to
--
cto :: Floating u => Point2 u -> Radian -> Point2 u -> Radian -> Curve u
cto start cin end cout = Curve start (start .+^ v1) (end .+^ v2) end
  where
    sz     = 0.375 * (vlength $ pvec start end)
    v1     = avec cin  sz
    v2     = avec cout sz



incidenceL :: (Real u, Floating u) => Line u -> Radian
incidenceL (Line p1 p2) = direction (pvec p2 p1)

-- incidence on a curve is not the same as the incidence of the
-- repective control point to the end point...


--------------------------------------------------------------------------------
--



-- | midpoint between two points
--
pointMidpoint :: Fractional u => Point2 u -> Point2 u -> Point2 u
pointMidpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0



-- | subdivide with an affine weight along the line...
--
subdividet :: Real u
           => u -> Curve u -> (Curve u, Curve u)
subdividet t (Curve p0 p1 p2 p3) = 
    (Curve p0 p01 p012 p0123, Curve p0123 p123 p23 p3)
  where
    p01   = affineCombination t p0    p1
    p12   = affineCombination t p1    p2
    p23   = affineCombination t p2    p3
    p012  = affineCombination t p01   p12
    p123  = affineCombination t p12   p23
    p0123 = affineCombination t p012  p123

affineCombination :: Real u => u -> Point2 u -> Point2 u -> Point2 u
affineCombination a p1 p2 = p1 .+^ a *^ (p2 .-. p1)


--------------------------------------------------------------------------------
-- Curve length

curveLength :: (Floating u, Ord u, InnerSpace (Vec2 u))
               => Curve u -> u
curveLength = gravesenLength 0.1

-- | Jens Gravesen\'s bezier arc-length approximation. 
--
-- Note this implementation is parametrized on error tolerance.
--
gravesenLength :: (Floating u, Ord u, InnerSpace (Vec2 u))
               => u -> Curve u -> u
gravesenLength err_tol crv = step crv where
  step c = let l1 = ctrlPolyLength c
               l0 = cordLength c
           in if   l1-l0 > err_tol
              then let (a,b) = subdivide c in step a + step b
              else 0.5*l0 + 0.5*l1


ctrlPolyLength :: (Floating u, InnerSpace (Vec2 u)) 
               => Curve u -> u
ctrlPolyLength (Curve p0 p1 p2 p3) = 
  distance p0 p1 + distance p1 p2 + distance p2 p3

cordLength ::(Floating u, InnerSpace (Vec2 u)) 
           => Curve u -> u
cordLength (Curve p0 _ _ p3) = distance p0 p3


--------------------------------------------------------------------------------
-- tangents

startTangent :: (Floating u, Real u) => Curve u -> Radian
startTangent (Curve p0 p1 _ _) = direction $ pvec p0 p1

endTangent :: (Floating u, Real u) => Curve u -> Radian
endTangent (Curve _ _ p2 p3) = direction $ pvec p3 p2
 
endDirection :: (Floating u, Real u) => Curve u -> Radian
endDirection = step . endTangent
  where
    step r | r < pi    = r + pi
           | otherwise = r - pi