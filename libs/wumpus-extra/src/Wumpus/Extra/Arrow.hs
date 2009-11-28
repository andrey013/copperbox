{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Arrow
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

module Wumpus.Extra.Arrow where

import Wumpus.Core


import Data.AffineSpace
import Data.Aviary


-- Still don't know what to do about line width...
type LineW u = u

-- Notes in TikZ - arrowheads must be able to 'shorten lines' 
-- (vis open triangle arrow heads - the line must be retracted).

tripoints :: Floating u
          => Radian -> u -> u -> Point2 u -> (Point2 u, Point2 u)
tripoints ang dist halfbase tip = (back .+^ v, back .-^ v)
  where
    back = tip .-^ (avec ang dist)
    v    = avec (ang + pi/2) halfbase




arrowTri' :: (Floating u, Real u) => Point2 u -> Point2 u -> Picture u
arrowTri' p1 p2 = frameMulti [thead, aline]
  where
    lwidth = 1.0
    ang    = langle p1 p2
    thead  = ahFilledTriRetro () lwidth ang p2
    aline  = ostroke () $ path p1 [lineTo $ p2 .-^ avec ang lwidth]
  

ahFilledTriangle :: (Floating u, Real u, Fill t)
                 => t -> LineW u -> Radian -> Point2 u -> Primitive u
ahFilledTriangle attr lw ang pt = fill attr $ vertexPath [pt,u,v]
  where
    (u,v) = tripoints ang (lw*10) (lw*5) pt

ahFilledTriRetro :: (Floating u, Real u, Fill t)
                 => t -> LineW u -> Radian -> Point2 u -> Primitive u
ahFilledTriRetro attr lw ang pt = fill attr $ vertexPath [pt,u,retro,v]
  where
    (u,v) = tripoints ang (lw*10) (lw*5) pt
    retro = pt .-^ avec ang (lw*5)

-- Old...





newtype Arrow u = Arrow { arrowPaths :: [Primitive u] }

type DArrow = Arrow Double




picArrow :: (Fractional u, Ord u) => Arrow u -> Picture u
picArrow (Arrow xs) = multi $ map frame xs

arrow :: (Floating u, Real u) => Point2 u -> Point2 u -> Arrow u
arrow = arrowline (return `oo` arrowheadVee 10 (pi/10))


arrowline :: (Floating u, Real u) 
          => (Radian -> Point2 u -> [Path u]) 
          -> Point2 u
          -> Point2 u 
          -> Arrow u
arrowline mk p p' = Arrow (map zostroke $ path1:tip) where
  path1 = vertexPath [p,p']
  theta = langle p p'
  tip   = mk theta p'



arrowheadVee :: (Floating u, Real u)
             => u 
             -> Radian
             -> (Radian -> Point2 u -> Path u)
arrowheadVee d ang = 
  \theta endpt -> let p0   = endpt .+^ (hvec (-d))
                      p01  = rotateAbout (pi-ang) endpt p0
                      p02  = rotateAbout (pi+ang) endpt p0
                      px   = vertexPath [p01,endpt,p02]
                  in pointwise (rotateAbout (theta - pi) endpt) px

arrowPerp :: (Floating u, Real u) => Point2 u -> Point2 u -> Arrow u
arrowPerp = arrowline (return `oo` arrowheadPerp 10)


arrowheadPerp :: (Floating u, Real u) => u -> (Radian -> Point2 u -> Path u)
arrowheadPerp d = 
  \theta endpt -> let p0   = endpt .+^ (hvec (-d))
                      p1   = endpt .+^ (hvec d)
                      px   = vertexPath [p0,p1]
                  in pointwise (rotateAbout (theta+pi/2) endpt) px

arrowTri :: (Floating u, Real u) => Point2 u -> Point2 u -> Arrow u
arrowTri = arrowline (return `oo` arrowheadVee 10 (pi/10))



--------------------------------------------------------------------------------
