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
  , strokeEllipse
  , fillEllipse

  ) where


import Wumpus.Shapes.Base
import Wumpus.Shapes.Utils

import Wumpus.Core hiding ( Ellipse, ellipse )        -- package: wumpus-core
import Wumpus.Basic.Anchors                           -- package: wumpus-basic
import Wumpus.Basic.Graphic


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


-- helper
updateCTM :: (CTM u -> CTM u) -> Ellipse u -> Ellipse u
updateCTM f = star (\s m -> s { ell_ctm = f m } ) ell_ctm

instance (Real u, Floating u) => Rotate (Ellipse u) where
  rotate r = updateCTM (rotateCTM r)

instance Num u => Scale (Ellipse u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Ellipse u) where
  translate x y = updateCTM (translateCTM x y)


instance AddLabel (Ellipse u) where
  ell `addLabel` lbl = ell { ell_label = Just lbl }
     


--------------------------------------------------------------------------------
-- 

ellipse :: Num u => u -> u -> Ellipse u
ellipse rx ry = Ellipse { ell_radius_x = rx
                        , ell_radius_y = ry
                        , ell_ctm      = identityCTM
                        , ell_label    = Nothing }
 

-- Hand-build the circle with bezier arcs.
--
-- This makes a better drawing than using PostScript\'s @arc@
-- command (via ellipse in wumpus-core) as line width does not
-- get affected by the CTM.
--
drawEllipse :: (Real u, Floating u) 
           => (Path u -> Primitive u) -> Ellipse u -> Graphic u
drawEllipse drawF ell = labelpic . ellpic
  where
    labelpic = maybe id (labelGraphic (ell_ctm ell)) $ ell_label ell
    ellpic  = wrapG $ drawF $ curvedPath $ ellipsePoints ell


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


strokeEllipse :: (Real u, Floating u, Stroke t) 
             => t -> Ellipse u -> Graphic u
strokeEllipse t  = drawEllipse (cstroke t)

fillEllipse :: (Real u, Floating u, Fill t) 
             => t -> Ellipse u -> Graphic u
fillEllipse t  = drawEllipse (fill t)

instance (Real u, Floating u) => DrawShape (Ellipse u) where
  strokeShape t = drawEllipse (cstroke t) 
  fillShape   t = drawEllipse (fill t)
