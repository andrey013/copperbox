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
import Wumpus.Shapes.Utils

import Wumpus.Core hiding ( Ellipse, ellipse )        -- package: wumpus-core
import Wumpus.Basic.Anchors                           -- package: wumpus-basic
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing

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
updateCTM f = star (\s m -> s { ell_ctm = f m } ) ell_ctm


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
 


strokeE :: (Real u, Floating u)
        => DrawingAttr -> Point2 u -> Ellipse u -> Graphic u
strokeE attr (P2 x y) = 
    wrapG . cstroke (strokeAttr attr) . ellipsePath . translate x y 


fillE :: (Real u, Floating u) 
      => DrawingAttr -> Point2 u -> Ellipse u -> Graphic u
fillE attr (P2 x y) = 
    wrapG . fill (fillAttr attr) . ellipsePath . translate x y

textE :: (Real u, Floating u, FromPtSize u) 
      => DrawingAttr -> Point2 u -> Ellipse u -> Graphic u
textE attr (P2 x y) ell = maybe id sk $ ell_label ell
  where
    ctm      = ell_ctm $ translate x y ell
    sk label = labelGraphic label (textAttr attr) ctm 


make :: (Real u, Floating u) 
     => DrawingAttr -> Point2 u -> Ellipse u -> Ellipse u
make _ (P2 x y) = translate x y


instance (Real u, Floating u, FromPtSize u) => Draw (Ellipse u) where
  draw ell = AGraphic id df (\a p -> make a p ell)
    where
      df attr pt = textE attr pt ell . strokeE attr pt ell
                                     . fillE attr pt ell


{-
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
-}

ellipsePath :: (Real u, Floating u) => Ellipse u -> Path u
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


