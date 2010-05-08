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

import Wumpus.Core

import Wumpus.Extra.BasicObjects
import Wumpus.Extra.Marks
import Wumpus.Extra.Utils

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

  

filledStraightTriangle :: (Floating u, Real u, Mark t)
                       => t -> Radian -> Point2 u -> Primitive u
filledStraightTriangle attr ang pt = mfill attr $ vertexPath [pt,u,v]
  where
    (u,v) = tripoints ang (lwX10 attr) (lwX5 attr) pt


strokedCurvedTip :: (Floating u, Real u, Ord u, InnerSpace (Vec2 u), Mark t) 
                 => t -> Radian -> Point2 u -> Primitive u
strokedCurvedTip attr ang pt = mostroke attr $ curvesToPath [c1,c2]
  where
    (u,v) = tripoints ang (lwX10 attr) (lwX5 attr) pt
    iang  = ang+pi
    c1    = bend (iang+pi/2) iang u pt
    c2    = bend iang (iang-pi/2) pt v

    -- bend not intuitive here...

{-

ahFilledTriRetro :: (Floating u, Real u, Mark t)
                 => t -> LineW u -> Radian -> Point2 u -> Primitive u
ahFilledTriRetro attr ang pt = mfill attr $ vertexPath [pt,u,retro,v]
  where
    (u,v) = tripoints ang (lwX10 attr) (lwX5 attr) pt
    retro = pt .-^ avec ang (lwX5 attr)
-}

doubleHook :: (Floating u, Real u, Ord u, InnerSpace (Vec2 u), Mark t) 
           => t -> Radian -> Point2 u -> Primitive u
doubleHook attr ang pt = mostroke attr $ curvesToPath [c,c']
  where
    theta = pi/3
    alpha = pi-theta
    vec   = avec ang (lwX5 attr)
    c     = rotateAbout (-pi/2) pt $ bend theta alpha (pt .+^ vec) pt
    c'    = rotateAbout (-pi/2) pt $ bend theta alpha pt (pt .-^ vec)

straightPerp :: (Floating u, Ord u, Mark t) 
             => t -> Radian -> Point2 u -> Primitive u
straightPerp attr ang pt = 
    mostroke attr $ lineSegmentToPath $ lineSegment p1 p2
  where
    (p1,p2)  = pointsPerp ang (lwX5 attr) pt



bracketTip :: (Floating u, Ord u, Mark t)
           => t -> Radian -> Point2 u -> Primitive u
bracketTip attr ang pt = 
    mostroke attr $ vertexPath [ p1 .-^ vec, p1, p2, p2 .-^ vec ]
  where
    vec      = avec ang (lwX5 attr) 
    (p1,p2)  = pointsPerp ang (lwX5 attr) pt

outwardBracketTip :: (Floating u, Ord u, Mark t)
                  => t -> Radian -> Point2 u -> Primitive u
outwardBracketTip attr ang pt = 
    mostroke attr $ vertexPath [ p1 .+^ vec, p1, p2, p2 .+^ vec ]
  where
    vec      = avec ang (lwX5 attr) 
    (p1,p2)  = pointsPerp ang (lwX5 attr) pt


