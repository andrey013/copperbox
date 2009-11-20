{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Arrow
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Extra.Arrow where

import Wumpus.Core


import Data.AffineSpace
import Data.Aviary

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
