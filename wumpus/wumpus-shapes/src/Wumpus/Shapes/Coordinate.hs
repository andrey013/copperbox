{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.Coordinate
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Coordinate points
--
-- Note - coordinate points cannot have labels.
-- 
--------------------------------------------------------------------------------

module Wumpus.Shapes.Coordinate
  ( 

    Coordinate(..)
  , DCoordinate
  , coordinate
  , drawCoordinate

  ) where

import Wumpus.Shapes.Base
import Wumpus.Shapes.Utils

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Anchors             -- package: wumpus-basic
import Wumpus.Basic.Graphic

--------------------------------------------------------------------------------

-- | Coordinates

data Coordinate u = Coordinate
      { coord_ctm             :: CTM u 
      }

type DCoordinate = Coordinate Double

type instance DUnit (Coordinate u) = u


-- Instances 

instance (Real u, Floating u) => AnchorCenter (Coordinate u) where
  center = ctmCenter . coord_ctm


instance (Floating u, Real u) => Rotate (Coordinate u) where
  rotate r = star (\s m -> s { coord_ctm = rotateCTM r m }) coord_ctm 

instance Num u => Scale (Coordinate u) where
  scale x y = star (\s m -> s { coord_ctm = scaleCTM x y m }) coord_ctm

instance Num u => Translate (Coordinate u) where
  translate x y = star (\s m -> s { coord_ctm = translateCTM x y m }) coord_ctm


--

coordinate :: Num u => Coordinate u
coordinate = Coordinate identityCTM

-- Note - currently this takes no notice of any scaling 
-- transformations in the CTM...
--
drawCoordinate :: (Real u, Floating u, Ellipse t) 
               => t -> Coordinate u -> Graphic u
drawCoordinate t coord = wrapG $ ellipse t 2 2 (center coord)

