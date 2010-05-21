{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.Coordinate
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Coordinate points
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Shape.Coordinate
  ( 

    Coordinate(..)
  , coordinate
  , drawCoordinate

  ) where

import Wumpus.Core hiding ( CTM )
import Wumpus.Extra.Shape.Base
import Wumpus.Extra.Utils


--------------------------------------------------------------------------------

-- | Coordinates

data Coordinate u = Coordinate
      { coord_center          :: Point2 u
      , coord_ctm             :: CTM u 
      }

type instance DUnit (Coordinate u) = u

-- helper - extract coord_center w.r.t. the CTM 
--
inCtxCoord :: Num u => Coordinate u -> (Point2 u -> a) -> a
inCtxCoord coord f = f $ ctm *# pt
    where
      ctm       = coord_ctm    coord
      pt        = coord_center coord

 

-- Instances 

instance Num u => AnchorCenter (Coordinate u) where
  center c = inCtxCoord c id


instance (Floating u, Real u) => Rotate (Coordinate u) where
  rotate r = pstar (\m s -> s { coord_ctm = rotateCTM r m }) coord_ctm 

instance (Floating u, Real u) => RotateAbout (Coordinate u) where
  rotateAbout r pt = 
      pstar (\m s -> s { coord_ctm = rotateAboutCTM r pt m }) coord_ctm

instance Num u => Scale (Coordinate u) where
  scale x y = pstar (\m s -> s { coord_ctm = scaleCTM x y m }) coord_ctm

instance Num u => Translate (Coordinate u) where
  translate x y = 
     pstar (\m s -> s { coord_ctm = translateCTM x y m }) coord_ctm


--

coordinate :: Num u => Point2 u -> Coordinate u
coordinate pt = Coordinate pt identityMatrix

drawCoordinate :: (Fractional u, Ellipse t) => t -> Coordinate u -> Primitive u
drawCoordinate t coord = ellipse t 2 2 (inCtxCoord coord id)

