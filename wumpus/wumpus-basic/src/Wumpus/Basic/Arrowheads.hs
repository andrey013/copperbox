{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Arrowheads
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

module Wumpus.Basic.Arrowheads
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

import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space


tripoints :: Floating u
          => Radian -> u -> Radian -> Point2 u -> (Point2 u, Point2 u)
tripoints triang xchar_height theta tip = (tip .-^ v1, tip .-^ v2)
  where
    halfang = 0.5 * triang
    d       = xchar_height / (fromRadian $ cos halfang)
    v1      = avec (theta + halfang) d
    v2      = avec (theta - halfang) d



-- width = xchar_height
-- filled with stroke colour!

triAng :: (Floating u, Real u, FromPtSize u)
      => Radian 
      -> (DrawingAttr -> Path u -> Primitive u) 
      -> DrawingAttr -> Radian -> GraphicF u
triAng ang fn attr theta pt = wrapG $ fn attr $ vertexPath [pt,u,v]
  where
    sz    = fromPtSize $ xcharHeight $ font_size $ font_props attr
    (u,v) = tripoints ang sz theta  pt



tri90 :: (Floating u, Real u, FromPtSize u)
      => DrawingAttr -> Radian -> GraphicF u
tri90 = triAng (pi/2) (\a p -> fill (stroke_colour a) p)


tri60 :: (Floating u, Real u, FromPtSize u)
      => DrawingAttr -> Radian -> GraphicF u
tri60 = triAng (pi/3) (\a p -> fill (stroke_colour a) p)

tri45 :: (Floating u, Real u, FromPtSize u)
      => DrawingAttr -> Radian -> GraphicF u
tri45 = triAng (pi/4) (\a p -> fill (stroke_colour a) p)


otri90 :: (Floating u, Real u, FromPtSize u)
      => DrawingAttr -> Radian -> GraphicF u
otri90 = triAng (pi/2) (\a p -> cstroke (strokeAttr a) p)

otri60 :: (Floating u, Real u, FromPtSize u)
      => DrawingAttr -> Radian -> GraphicF u
otri60 = triAng (pi/3) (\a p -> cstroke (strokeAttr a) p)

otri45 :: (Floating u, Real u, FromPtSize u)
      => DrawingAttr -> Radian -> GraphicF u
otri45 = triAng (pi/4) (\a p -> cstroke (strokeAttr a) p)


barbAng :: (Floating u, Real u, FromPtSize u)
      => Radian -> DrawingAttr -> Radian -> GraphicF u
barbAng ang attr theta pt = 
    wrapG $ ostroke (strokeAttr attr) $ vertexPath [u,pt,v]
  where
    sz    = fromPtSize $ xcharHeight $ font_size $ font_props attr
    (u,v) = tripoints ang sz theta  pt


barb90 :: (Floating u, Real u, FromPtSize u) 
       => DrawingAttr -> Radian -> GraphicF u
barb90 = barbAng (pi/2)

barb60 :: (Floating u, Real u, FromPtSize u) 
       => DrawingAttr -> Radian -> GraphicF u
barb60 = barbAng (pi/3)

barb45 :: (Floating u, Real u, FromPtSize u) 
       => DrawingAttr -> Radian -> GraphicF u
barb45 = barbAng (pi/4)



perp :: (Floating u, FromPtSize u) => DrawingAttr -> Radian -> GraphicF u
perp attr theta = \pt -> 
    wrapG $ ostroke (strokeAttr attr) $ vertexPath [ pt .+^ v, pt .-^ v]
  where
    half_sz = 0.5 * (fromPtSize $ xcharHeight $ font_size $ font_props attr)
    v       = avec (theta + pi/2) half_sz