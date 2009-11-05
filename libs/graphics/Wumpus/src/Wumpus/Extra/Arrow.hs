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

import Wumpus.Core.AffineTrans
import Wumpus.Core.Geometry
import Wumpus.Core.Picture

import Data.FunctionExtras
import Data.AffineSpace

newtype Arrow u = Arrow { arrowPaths :: [Path u] }  -- to do [(Props,Path u)]

type DArrow = Arrow Double

picArrow :: (Num u, Ord u) => Arrow u -> Picture u
picArrow (Arrow xs) = zmultipath xs

arrow :: (Floating u, Real u) => Point2 u -> Point2 u -> Arrow u
arrow = arrowline (return `oo` arrowheadVee False 10 (pi/10))


arrowline :: (Floating u, Real u) 
          => (Radian -> Point2 u -> [Path u]) 
          -> Point2 u
          -> Point2 u 
          -> Arrow u
arrowline mk p p' = Arrow (Path p [PLine p'] : tip) where
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
                      styl = if filled then CFill else OStroke
                      path = Path p01 (map PLine [endpt,p02])
                  in pointwise (rotateAbout (theta - pi) endpt) path

arrowPerp :: (Floating u, Real u) => Point2 u -> Point2 u -> Arrow u
arrowPerp = arrowline (return `oo` arrowheadPerp 10)


arrowheadPerp :: (Floating u, Real u) => u -> (Radian -> Point2 u -> Path u)
arrowheadPerp d = 
  \theta endpt -> let p0   = endpt .+^ (hvec (-d))
                      p1   = endpt .+^ (hvec d)
                      path = Path p0 [PLine p1]
                  in pointwise (rotateAbout (theta+pi/2) endpt) path

arrowTri :: (Floating u, Real u) => Point2 u -> Point2 u -> Arrow u
arrowTri = arrowline (return `oo` arrowheadVee True 10 (pi/10))



--------------------------------------------------------------------------------
