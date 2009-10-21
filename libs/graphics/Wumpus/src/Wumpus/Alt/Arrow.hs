{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Alt.Arrow
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Alt.Arrow where

import Wumpus.Alt.Geometry
import Wumpus.Alt.Picture

import Data.FunctionExtras
import Data.AffineSpace

newtype Arrow u = Arrow { arrowPaths :: [Path u] }

type DArrow = Arrow Double

picArrow :: (Num u, Ord u) => Arrow u -> Picture u
picArrow (Arrow xs) = picMultiPath xs

arrow :: (Floating u, Real u) => Point2 u -> Point2 u -> Arrow u
arrow = arrowline (return `oo` arrowheadVee False 10 (pi/10))


arrowline :: (Floating u, Real u) 
          => (Radian -> Point2 u -> [Path u]) 
          -> Point2 u
          -> Point2 u 
          -> Arrow u
arrowline mk p p' = Arrow (Path OpenStroke p [Ls p'] : tip) where
  theta = langle p p'
  tip   = mk theta p'



arrowheadVee :: (Floating u, Real u)
             => Bool 
             -> u 
             -> Radian
             -> (Radian -> Point2 u -> Path u)
arrowheadVee filled d ang = 
  \theta endpt -> let p0   = endpt .+^ (hvec (-d))
                      p01  = rotateAbout (pi-ang) endpt p0
                      p02  = rotateAbout (pi+ang) endpt p0
                      styl = if filled then Fill else OpenStroke
                      path = Path styl p01 (map Ls [endpt,p02])
                  in pointwise (rotateAbout (theta - pi) endpt) path

arrowPerp :: (Floating u, Real u) => Point2 u -> Point2 u -> Arrow u
arrowPerp = arrowline (return `oo` arrowheadPerp 10)


arrowheadPerp :: (Floating u, Real u) => u -> (Radian -> Point2 u -> Path u)
arrowheadPerp d = 
  \theta endpt -> let p0   = endpt .+^ (hvec (-d))
                      p1   = endpt .+^ (hvec d)
                      path = Path OpenStroke p0 [Ls p1]
                  in pointwise (rotateAbout (theta+pi/2) endpt) path

arrowTri :: (Floating u, Real u) => Point2 u -> Point2 u -> Arrow u
arrowTri = arrowline (return `oo` arrowheadVee True 10 (pi/10))



--------------------------------------------------------------------------------
