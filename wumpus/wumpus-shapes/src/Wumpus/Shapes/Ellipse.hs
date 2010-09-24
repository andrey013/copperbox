{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.Ellipse
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Coordinate points
-- 
--------------------------------------------------------------------------------

module Wumpus.Shapes.Ellipse
  ( 

    Ellipse(..)
  , DEllipse
  , ellipse
  , ellipse_

  ) where


import Wumpus.Shapes.Base

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Anchors                     -- package: wumpus-basic
import Wumpus.Basic.Graphic

import Control.Applicative
import Data.Monoid

--------------------------------------------------------------------------------

-- | Coordinates

data Ellipse u = Ellipse
      { ell_radius_x      :: u
      , ell_radius_y      :: u
      , ell_ctm           :: CTM u 
      , ell_label         :: Maybe ShapeLabel
      }

type DEllipse = Ellipse Double

type instance DUnit (Ellipse u) = u




-- ctr * CTM * radius      
--
withGeom :: Num u => (CTM u -> u -> u -> a) -> Ellipse u -> a
withGeom f (Ellipse { ell_ctm=ctm, ell_radius_x=rx, ell_radius_y=ry }) = 
    f ctm rx ry
     
calcPoint :: (Real u, Floating u) => (u -> u -> Vec2 u) -> Ellipse u -> Point2 u
calcPoint f = withGeom $ \ctm rx ry -> 
    let (V2 x y) = f rx ry in ctmDisplace x y ctm

updateCTM :: (CTM u -> CTM u) -> Ellipse u -> Ellipse u
updateCTM f = (\s m -> s { ell_ctm = f m } ) <*> ell_ctm


-------------------------------------------------------------------------------- 

-- Instances 

instance (Real u, Floating u) => CenterAnchor (Ellipse u) where
  center = ctmCenter . ell_ctm

instance (Real u, Floating u) => CardinalAnchor (Ellipse u) where
  north = calcPoint $ \ _  ry -> vvec ry
  south = calcPoint $ \ _  ry -> vvec (-ry)
  east  = calcPoint $ \ rx _  -> hvec rx
  west  = calcPoint $ \ rx _  -> hvec (-rx)

instance (Real u, Floating u) => CardinalAnchor2 (Ellipse u) where
  northeast = calcPoint $ \rx ry -> rescale rx ry $ avec (0.25*pi) rx
  southeast = calcPoint $ \rx ry -> rescale rx ry $ avec (1.75*pi) rx
  southwest = calcPoint $ \rx ry -> rescale rx ry $ avec (0.75*pi) rx
  northwest = calcPoint $ \rx ry -> rescale rx ry $ avec (1.25*pi) rx


instance (Real u, Floating u) => Rotate (Ellipse u) where
  rotate r = updateCTM (rotateCTM r)

instance Num u => Scale (Ellipse u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Ellipse u) where
  translate x y = updateCTM (translateCTM x y)



--------------------------------------------------------------------------------
-- 

ellipse :: Num u => u -> u -> Ellipse u
ellipse rx ry = Ellipse { ell_radius_x = rx
                        , ell_radius_y = ry
                        , ell_ctm      = identityCTM
                        , ell_label    = Nothing }
 

ellipse_ :: Num u => u -> u -> String -> Ellipse u
ellipse_ rx ry str = (ellipse rx ry) { ell_label = Just $ ShapeLabel str }
 


strokeE :: (Real u, Floating u) => Ellipse u -> Graphic u
strokeE = closedStroke . ellipsePath


borderedE :: (Real u, Floating u) => Ellipse u -> Graphic u
borderedE = borderedPath . ellipsePath 



textE :: (Real u, Floating u, FromPtSize u) 
      => Ellipse u -> Graphic u
textE ell = maybe mempty sk $ ell_label ell
  where
    sk label = labelGraphic label (ell_ctm ell) 

instance (Real u, Floating u, FromPtSize u) => DrawShape (Ellipse u) where
  drawShape ell = intoImage (pureDF ell) (borderedE ell `mappend` textE ell)

instance (Real u, Floating u, FromPtSize u) => OutlineShape (Ellipse u) where
  outlineShape ell = intoImage (pureDF ell) (strokeE ell `mappend` textE ell)



ellipsePath :: (Real u, Floating u) => Ellipse u -> PrimPath u
ellipsePath = curvedPath . ellipsePoints

ellipsePoints :: (Real u, Floating u) => Ellipse u -> [Point2 u]
ellipsePoints (Ellipse { ell_ctm=ctm, ell_radius_x=rx, ell_radius_y=ry }) = 
    map fn all_points
  where
    fn (P2 x y) = ctmDisplace x y ctm
    all_points  = map (rescale rx ry) $ bezierCircle 2 rx zeroPt 


-- | x_radius is the unit length.
--
rescale :: (Scale t, Fractional u, u ~ DUnit t) => u -> u -> t -> t
rescale rx ry = scale 1 (ry/rx) 


