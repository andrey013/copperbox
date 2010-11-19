{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Shapes.Coordinate
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- A Coordinate is operationally like a shape but it can only be 
-- drawn as a dot or a cross and it only supports @CenterAnchor@. 
--
-- Coordinates support affine transformations, however 
-- transfomations only displace a coordinate\'s origin they do 
-- not change how it is drawn (one cannot elongate the drawing of 
-- a coordinate with a scale). This is why coordinates are not 
-- Shapes, though one major use of coordinates is to illustrate 
-- anchor points on Shapes.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes.Coordinate
  (
    CoordinateAnchor
  , DCoordinateAnchor
  , Coordinate
  , DCoordinate
  , coordinate

  , coordinateDot
  , coordinateX

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Shapes.Base

import Wumpus.Core                              -- package: wumpus-core

import Control.Monad

-- Note - a CoordinateAnchor should _NOT_ have affine instances.
-- 
-- However, a coordinate supports affine transformations. This 
-- follows the logic where Shapes transform but representations 
-- of their anchors (i.e. the concrete types Rectangle, Circle...)
-- do not transform. 
-- 


--------------------------------------------------------------------------------
-- | Coordinate

newtype CoordinateAnchor u = CoordinateAnchor { getCoordAnchor :: ShapeCTM u }
  deriving (Eq,Ord,Show)

type DCoordinateAnchor = CoordinateAnchor Double

type instance DUnit (CoordinateAnchor u) = u

newtype Coordinate u = Coordinate { getCoordinate :: CoordinateAnchor u }
  deriving (Eq,Ord,Show)

type DCoordinate = Coordinate Double

type instance DUnit (Coordinate u) = u

type LocCoordinate u = Point2 u -> Coordinate u



runCoordinate :: ShapeGeom u a -> CoordinateAnchor u -> a
runCoordinate mf a = runShapeGeom (getCoordAnchor a) mf 


instance (Real u, Floating u) => CenterAnchor (CoordinateAnchor u) where
  center = runCoordinate shapeCenter

cMap :: (ShapeCTM u -> ShapeCTM u) -> Coordinate u -> Coordinate u
cMap fn = Coordinate . CoordinateAnchor . fn . getCoordAnchor . getCoordinate


-- Affine instances

instance (Real u, Floating u) => Rotate (Coordinate u) where
  rotate r = cMap (rotate r)

instance (Real u, Floating u) => RotateAbout (Coordinate u) where
  rotateAbout r pt = cMap (rotateAbout r pt)

instance Num u => Scale (Coordinate u) where
  scale sx sy = cMap (scale sx sy)

instance Num u => Translate (Coordinate u) where
  translate dx dy = cMap (translate dx dy)



coordinate :: Num u => LocCoordinate u
coordinate = Coordinate . CoordinateAnchor . makeShapeCTM


coordinateDot :: (Real u, Floating u, FromPtSize u) 
              => Coordinate u -> Image u (CoordinateAnchor u)
coordinateDot x = intoImage (return $ getCoordinate x) (drawDot x)



-- | Note - the @x@ is drawn /regardless/ of any scaling or rotation.
--
coordinateX :: (Real u, Floating u, FromPtSize u) 
            => Coordinate u -> Image u (CoordinateAnchor u)
coordinateX x = intoImage (return $ getCoordinate x) (drawX x)



quarterMarkHeight :: (Fractional u, FromPtSize u) => CF u
quarterMarkHeight = liftM (0.25*) markHeight

drawDot :: (Real u, Floating u, FromPtSize u) => Coordinate u -> Graphic u
drawDot coord = quarterMarkHeight >>= \qh -> 
    localize bothStrokeColour (filledEllipse qh qh `at` ctr)
  where
    ctr = center $ getCoordinate coord

drawX :: (Real u, Floating u, FromPtSize u) => Coordinate u -> Graphic u
drawX coord = quarterMarkHeight >>= \qh -> line1 qh `oplus` line2 qh
  where
    P2 x y  = center $ getCoordinate coord
    line1 h = straightLineBetween (P2 (x-h) (y-h)) (P2 (x+h) (y+h))
    line2 h = straightLineBetween (P2 (x+h) (y-h)) (P2 (x-h) (y+h))

