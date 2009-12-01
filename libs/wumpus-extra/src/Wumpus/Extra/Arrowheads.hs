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
  , doubleHook
  , straightPerp

  ) where

import Wumpus.Extra.Lines
import Wumpus.Geometry
-- import Wumpus.Geometry.CoreAdditions
import Wumpus.Core

import Data.Aviary

import Data.AffineSpace
import Data.VectorSpace




tripoints :: Floating u
          => Radian -> u -> u -> Point2 u -> (Point2 u, Point2 u)
tripoints ang dist halfbase tip = (back .+^ v, back .-^ v)
  where
    back = tip .-^ (avec ang dist)
    v    = avec (ang + pi/2) halfbase



  

filledStraightTriangle :: (Floating u, Real u, Fill t)
                       => t -> u -> Radian -> Point2 u -> Primitive u
filledStraightTriangle attr lw ang pt = fill attr $ vertexPath [pt,u,v]
  where
    (u,v) = tripoints ang (lw*10) (lw*5) pt


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
    c  = rotateAbout (-pi/2) pt $ bend (pi/3) (pi/3) (pt .+^ avec ang (lw*5)) pt
    c' = rotateAbout (-pi/2) pt $ bend (pi/3) (pi/3) pt (pt .-^ avec ang (lw*5))

straightPerp :: (Floating u, Ord u, Stroke t) 
             => t -> u -> Radian -> Point2 u -> Primitive u
straightPerp attr lw ang pt = 
    ostroke attr $ lineSegmentToPath $ alineSegment (ang - pi/2) (lw*10) p1
  where
    p1 = pt .+^ ccwPerp ang (lw*5)

ccwPerp :: Floating u => Radian -> u -> Vec2 u
ccwPerp = appro avec (+ pi/2) id



