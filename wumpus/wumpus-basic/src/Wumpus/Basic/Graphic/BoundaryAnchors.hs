{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.BoundaryAnchors
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Composition operators for Drawings.
--
-- Note - some operations can produce empty drawings...
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.BoundaryAnchors
  (
  
    boundaryExtr
  , boundaryCenter

  , boundaryN
  , boundaryS
  , boundaryE
  , boundaryW
  
  , boundaryNE
  , boundaryNW
  , boundarySE
  , boundarySW

  , boundaryLeftEdge
  , boundaryRightEdge
  , boundaryBottomEdge
  , boundaryTopEdge

  ) where

import Wumpus.Basic.Graphic.Anchors

import Wumpus.Core                              -- package: wumpus-core

  


boundaryExtr :: (Boundary t, u ~ DUnit t) => (BoundingBox u -> a) -> t -> a
boundaryExtr f = f . boundary

-- Operations on bounds

-- | The center of a picture.
boundaryCenter :: (Boundary t, Fractional u, u ~ DUnit t) => t -> Point2 u
boundaryCenter = boundaryExtr center




-- | Extract the mid point of the top edge.
--
boundaryN :: (Boundary t, Fractional u, u ~ DUnit t) => t -> Point2 u
boundaryN = boundaryExtr north

-- | Extract the mid point of the bottom edge.
--
boundaryS :: (Boundary t, Fractional u, u ~ DUnit t) => t -> Point2 u
boundaryS = boundaryExtr south

-- | Extract the mid point of the left edge.
--
boundaryE :: (Boundary t, Fractional u, u ~ DUnit t) => t -> Point2 u
boundaryE = boundaryExtr east

-- | Extract the mid point of the right edge.
--
boundaryW :: (Boundary t, Fractional u, u ~ DUnit t) => t -> Point2 u
boundaryW = boundaryExtr west







-- | Extract the top-left corner.
--
boundaryNW :: (Boundary t, Fractional u, u ~ DUnit t) => t -> Point2 u
boundaryNW = boundaryExtr northwest

-- | Extract the top-right corner.
--
boundaryNE :: (Boundary t, u ~ DUnit t) => t -> Point2 u
boundaryNE = boundaryExtr ur_corner

-- | Extract the bottom-left corner.
--
boundarySW :: (Boundary t, u ~ DUnit t) => t -> Point2 u
boundarySW = boundaryExtr ll_corner

-- | Extract the bottom-right corner.
--
boundarySE :: (Boundary t, Fractional u, u ~ DUnit t) => t -> Point2 u
boundarySE = boundaryExtr southeast





boundaryLeftEdge :: (Boundary t, u ~ DUnit t) => t -> u
boundaryLeftEdge = boundaryExtr (point_x . ll_corner)

boundaryRightEdge :: (Boundary t, u ~ DUnit t) => t -> u
boundaryRightEdge = boundaryExtr (point_x . ur_corner)

boundaryBottomEdge :: (Boundary t, u ~ DUnit t) => t -> u
boundaryBottomEdge = boundaryExtr (point_y . ll_corner)


boundaryTopEdge :: (Boundary t, u ~ DUnit t) => t -> u
boundaryTopEdge = boundaryExtr (point_y . ur_corner)

