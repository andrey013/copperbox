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

  ) where

import Wumpus.Shapes.Base

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Anchors                     -- package: wumpus-basic
import Wumpus.Basic.Graphic

import Control.Applicative

--------------------------------------------------------------------------------

-- | Coordinates

data Coordinate u = Coordinate
      { coord_ctm             :: CTM u 
      }

type DCoordinate = Coordinate Double

type instance DUnit (Coordinate u) = u



updateCTM :: (CTM u -> CTM u) -> Coordinate u -> Coordinate u
updateCTM f = (\s i -> s { coord_ctm = f i}) <*> coord_ctm 

-- Instances 

instance (Real u, Floating u) => CenterAnchor (Coordinate u) where
  center = ctmCenter . coord_ctm


instance (Floating u, Real u) => Rotate (Coordinate u) where
  rotate r = updateCTM (rotateCTM r)

instance Num u => Scale (Coordinate u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Coordinate u) where
  translate x y = updateCTM (translateCTM x y)


--

coordinate :: Num u => Coordinate u
coordinate = Coordinate identityCTM


instance (Real u, Floating u) => DrawShape (Coordinate u) where
  drawShape coord = intoImage (pureDF coord) (drawC coord)
     

-- Note - this takes no notice of any scaling 
-- transformations in the CTM...
--
drawC :: (Real u, Floating u) 
      => Coordinate u -> Graphic u
drawC coord = filledEllipse 2 2 (center coord)

