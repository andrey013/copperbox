{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Arrows.Tips
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Anchor points on shapes.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Arrows.Tips
  ( 


    tri90
  , tri60
  , tri45
  , otri90
  , otri60
  , otri45

  , barb90
  , barb60
  , barb45

  , perp

  ) where

import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.Graphic
import Wumpus.Basic.Graphic.Image

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import Control.Applicative

-- | tripoints takes dimensions from the xlowerHeight.
--
tripoints :: (Floating u, FromPtSize u)
          => Radian -> Radian -> LocDrawingObject u (Point2 u, Point2 u)
tripoints triang theta tip = 
    (\h -> let d = h / (fromRadian $ cos halfang) 
           in (tip .-^ v1 d, tip .-^ v2 d))
      <$> asksObj lowerxHeight
  where
    halfang = 0.5 * triang
    v1 d    = avec (theta + halfang) d
    v2 d    = avec (theta - halfang) d



-- width = xchar_height
-- filled with stroke colour!

triAng :: (Floating u, Real u, FromPtSize u)
      => Radian 
      -> Radian
      -> (PrimPath u -> Graphic u) 
      -> LocGraphic u
triAng triang theta gf pt = 
    tripoints triang theta pt >>= \(u,v) -> gf (vertexPath [pt,u,v])



-- TODO - maybe filling needs to use swapColours

tri90 :: (Floating u, Real u, FromPtSize u)
      => Radian -> LocGraphic u
tri90 theta = triAng (pi/2) theta filledPath


tri60 :: (Floating u, Real u, FromPtSize u)
      => Radian -> LocGraphic u
tri60 theta = triAng (pi/3) theta filledPath


tri45 :: (Floating u, Real u, FromPtSize u)
      => Radian -> LocGraphic u
tri45 theta = triAng (pi/4) theta filledPath


otri90 :: (Floating u, Real u, FromPtSize u)
      => Radian -> LocGraphic u
otri90 theta = triAng (pi/2) theta closedStroke


otri60 :: (Floating u, Real u, FromPtSize u)
      => Radian -> LocGraphic u
otri60 theta = triAng (pi/3) theta closedStroke

otri45 :: (Floating u, Real u, FromPtSize u)
      => Radian -> LocGraphic u
otri45 theta = triAng (pi/4) theta closedStroke


barbAng :: (Floating u, Real u, FromPtSize u)
      => Radian -> Radian -> LocGraphic u
barbAng ang theta pt = 
    tripoints ang theta pt >>= \(u,v) -> openStroke (vertexPath [pt,u,v])


barb90 :: (Floating u, Real u, FromPtSize u) 
       => Radian -> LocGraphic u
barb90 = barbAng (pi/2)

barb60 :: (Floating u, Real u, FromPtSize u) 
       => Radian -> LocGraphic u
barb60 = barbAng (pi/3)

barb45 :: (Floating u, Real u, FromPtSize u) 
       => Radian -> LocGraphic u
barb45 = barbAng (pi/4)



perp :: (Floating u, FromPtSize u) => Radian -> LocGraphic u
perp theta pt =  
    asksObj lowerxHeight >>= \ h -> 
    let v = makeV h in openStroke $ vertexPath [ pt .+^ v, pt .-^ v]
  where
    makeV h  = avec (theta + pi/2) (0.5 * h)