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
-- Coordinate is a bit like a shape but does not generate a path 
-- and cannot be scaled or rotated (it can be translated).
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes.Coordinate
  (
    CoordinateAnchor
  , DCoordinateAnchor
  , Coordinate
  , DCoordinate
  , coordinate

  , coordinateMark

  ) where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Graphic
import Wumpus.Basic.Shapes.Base

import Wumpus.Core                              -- package: wumpus-core





--------------------------------------------------------------------------------
-- | Coordinate

data CoordinateAnchor u = CoordinateAnchor
      { coord_x   :: !u
      , coord_y   :: !u
      }
  deriving (Eq,Ord,Show)

type DCoordinateAnchor = CoordinateAnchor Double

type instance DUnit (CoordinateAnchor u) = u

newtype Coordinate u = Coordinate { getCoordinate :: CoordinateAnchor u }
  deriving (Eq,Ord,Show)

type DCoordinate = Coordinate Double

type instance DUnit (Coordinate u) = u


instance (Real u, Floating u) => CenterAnchor (CoordinateAnchor u) where
  center (CoordinateAnchor x y) = P2 x y

instance Num u => Translate (Coordinate u) where
  translate dx dy = Coordinate . fn . getCoordinate 
    where 
      fn (CoordinateAnchor x y) = CoordinateAnchor (x+dx) (y+dy)

-- Need a differentiation at the type level between a coord
-- that can be translated (moved) and a coord that anchors...

coordinate :: Num u => Point2 u -> Coordinate u
coordinate (P2 x y) = Coordinate $ CoordinateAnchor { coord_x = x, coord_y = y }

-- Note - should @coordinate@ take a point, Shapes don\'t.


coordinateMark :: (Real u, Floating u) 
               => Coordinate u -> Image u (CoordinateAnchor u)
coordinateMark x = intoImage (return $ getCoordinate x) (drawCoord x)

drawCoord :: (Real u, Floating u) => Coordinate u -> Graphic u
drawCoord coord = 
   localize bothStrokeColour $ filledEllipse 2 2 (center $ getCoordinate coord)

