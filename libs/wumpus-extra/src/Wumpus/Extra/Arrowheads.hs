{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Arrowheads
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Arrowheads...
--
--------------------------------------------------------------------------------

module Wumpus.Extra.Arrowheads 
  (

    filledStraightTriangle
  , strokedCurvedTip
  , doubleHook
  , straightPerp
  , bracketTip
  , outwardBracketTip

  ) where

import Wumpus.Extra.Lines
import Wumpus.Geometry
import Wumpus.Core

import Data.Aviary

import Data.AffineSpace
import Data.VectorSpace


-- Helpers

tripoints :: Floating u
          => Radian -> u -> u -> Point2 u -> (Point2 u, Point2 u)
tripoints ang dist halfbase tip = (back .+^ v, back .-^ v)
  where
    back = tip .-^ (avec ang dist)
    v    = avec (ang + pi/2) halfbase

ccwPerp :: Floating u => Radian -> u -> Vec2 u
ccwPerp = appro avec (+ pi/2) id


pointsPerp :: Floating u 
           => Radian -> u -> Point2 u -> (Point2 u, Point2 u)
pointsPerp ang dist pt = (pt .+^ ccwPerp ang dist, pt .-^ ccwPerp ang dist)


--------------------------------------------------------------------------------
-- Arrowheads

  

filledStraightTriangle :: (Floating u, Real u, Fill t)
                       => t -> u -> Radian -> Point2 u -> Primitive u
filledStraightTriangle attr lw ang pt = fill attr $ vertexPath [pt,u,v]
  where
    (u,v) = tripoints ang (lw*10) (lw*5) pt


strokedCurvedTip :: (Floating u, Real u, Ord u, InnerSpace (Vec2 u), Stroke t) 
                 => t -> u -> Radian -> Point2 u -> Primitive u
strokedCurvedTip attr lw ang pt = ostroke attr $ curvesToPath [c1,c2]
  where
    (u,v) = tripoints ang (lw*10) (lw*5) pt
    iang  = ang+pi
    c1    = bend (iang+pi/2) iang u pt
    c2    = bend iang (iang-pi/2) pt v

    -- bend not intuitive here...

{-

ahFilledTriRetro :: (Floating u, Real u, Fill t)
                 => t -> LineW u -> Radian -> Point2 u -> Primitive u
ahFilledTriRetro attr lw ang pt = fill attr $ vertexPath [pt,u,retro,v]
  where
    (u,v) = tripoints ang (lw*10) (lw*5) pt
    retro = pt .-^ avec ang (lw*5)
-}

doubleHook :: (Floating u, Real u, Ord u, Stroke t, InnerSpace (Vec2 u)) 
       => t -> u -> Radian -> Point2 u -> Primitive u
doubleHook attr lw ang pt = ostroke attr $ curvesToPath [c,c']
  where
    theta = pi/3
    alpha = pi-theta
    c   = rotateAbout (-pi/2) pt $ bend theta alpha (pt .+^ avec ang (lw*5)) pt
    c'  = rotateAbout (-pi/2) pt $ bend theta alpha pt (pt .-^ avec ang (lw*5))

straightPerp :: (Floating u, Ord u, Stroke t) 
             => t -> u -> Radian -> Point2 u -> Primitive u
straightPerp attr lw ang pt = 
    ostroke attr $ lineSegmentToPath $ lineSegment p1 p2
  where
    (p1,p2)  = pointsPerp ang (lw*5) pt



bracketTip :: (Floating u, Ord u, Stroke t)
           => t -> u -> Radian -> Point2 u -> Primitive u
bracketTip attr lw ang pt = 
    ostroke attr $ vertexPath [ p1 .-^ vec, p1, p2, p2 .-^ vec ]
  where
    vec      = avec ang (lw*5) 
    (p1,p2)  = pointsPerp ang (lw*5) pt

outwardBracketTip :: (Floating u, Ord u, Stroke t)
                  => t -> u -> Radian -> Point2 u -> Primitive u
outwardBracketTip attr lw ang pt = 
    ostroke attr $ vertexPath [ p1 .+^ vec, p1, p2, p2 .+^ vec ]
  where
    vec      = avec ang (lw*5) 
    (p1,p2)  = pointsPerp ang (lw*5) pt


