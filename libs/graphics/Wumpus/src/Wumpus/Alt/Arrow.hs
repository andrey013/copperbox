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

import Data.AffineSpace


arrow :: DPoint2 -> DPoint2 -> [DPath]
arrow p p' = [Path OpenStroke p [Ls p'], tip] where
  theta = {- pi/2 + -} (langle p p') 
  tip   = arrowheadVee 10 (pi/10) theta p'



arrowheadVee :: Double -> Radian
             -> (Radian -> DPoint2 -> DPath)
arrowheadVee d ang = 
  \theta endpt -> let p0  = endpt .+^ (hvec (-d))
                      p01 = rotateAbout (pi-ang) endpt p0
                      p02 = rotateAbout (pi+ang) endpt p0
                      pth = Path OpenStroke p01 (map Ls [endpt,p02])
                  in pointwise (rotateAbout (theta - pi) endpt) pth




--------------------------------------------------------------------------------
