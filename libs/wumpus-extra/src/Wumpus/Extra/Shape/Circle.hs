{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.Circle
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

module Wumpus.Extra.Shape.Circle
  ( 

    Circle(..)
  , circle
  , strokeCircle
  , fillCircle

  ) where

import Wumpus.Core hiding ( CTM )
import Wumpus.Extra.Shape.Base
import Wumpus.Extra.Utils

import Data.AffineSpace                 -- package: vector-space

--------------------------------------------------------------------------------

-- | Coordinates

data Circle u = Circle
      { circle_radius          :: u
      , circle_center          :: Point2 u
      , circle_ctm             :: CTM u 
      , circle_label           :: Maybe ShapeLabel
      }

type instance DUnit (Circle u) = u



-- helper - extract coord_center w.r.t. the CTM 
--

withGeom :: Num u => (CTM u -> Point2 u -> u -> a) -> Circle u -> a
withGeom f c = f (circle_ctm c) (circle_center c) (circle_radius c)


-------------------------------------------------------------------------------- 

-- Instances 

instance Num u => AnchorCenter (Circle u) where
  center = withGeom $ \ctm ctr _ -> ctm *# ctr


instance Floating u => AnchorCardinal (Circle u) where
  north = withGeom $ \ctm ctr r -> ctm *# (ctr .+^ vvec r)
  south = withGeom $ \ctm ctr r -> ctm *# (ctr .-^ vvec r)
  east  = withGeom $ \ctm ctr r -> ctm *# (ctr .+^ hvec r)
  west  = withGeom $ \ctm ctr r -> ctm *# (ctr .-^ hvec r)

  northeast = withGeom $ \ctm ctr r -> ctm *# (ctr .+^ avec (pi*0.25) r)
  southeast = withGeom $ \ctm ctr r -> ctm *# (ctr .-^ avec (pi*0.75) r)
  southwest = withGeom $ \ctm ctr r -> ctm *# (ctr .-^ avec (pi*0.25) r)
  northwest = withGeom $ \ctm ctr r -> ctm *# (ctr .+^ avec (pi*0.75) r)


-- helper
updateCTM :: (CTM u -> CTM u) -> Circle u -> Circle u
updateCTM f = pstar (\m s -> s { circle_ctm = f m } ) circle_ctm


instance (Floating u, Real u) => Rotate (Circle u) where
  rotate r = updateCTM (rotateCTM r) 

instance (Floating u, Real u) => RotateAbout (Circle u) where
  rotateAbout r pt = updateCTM (rotateAboutCTM r pt)


-- For scaling /just/ change the ctm - but remember to 
-- apply the ctm to all points and control points before
-- drawing the circle.
--
instance Num u => Scale (Circle u) where
  scale x y = updateCTM (scaleCTM x y) 

instance Num u => Translate (Circle u) where
  translate x y = updateCTM (translateCTM x y)


instance AddLabel (Circle u) where
  r `addLabel` text = pstar fn circle_label r
    where
      fn Nothing    s = s { circle_label = Just $ basicLabel text }
      fn (Just lbl) s = s { circle_label = Just $ updateText text lbl } 
     


--------------------------------------------------------------------------------
-- 

circle :: Num u => u -> Point2 u -> Circle u
circle radius pt = Circle radius pt identityMatrix Nothing


-- Hand-build the circle with bezier arcs.
--
-- This makes a better drawing than using PostScript\'s @arc@
-- command (via ellipse in wumpus-core) as line width does not
-- get affected by the CTM.
--
drawCircle :: (Floating u, Fractional u, Ord u) 
           => (Path u -> Primitive u) -> Circle u -> Composite u
drawCircle drawF circ = 
    labelledComposite ctm ctr (circle_label circ) shape

  where
    ctm        = circle_ctm    circ
    ctr        = circle_center circ 
    circle_pts = map (ctm *#) $ bezierCircle 2 (circle_radius circ) ctr
    shape      = drawF $ curvedPath circle_pts



strokeCircle :: (Floating u, Fractional u, Ord u, Stroke t) 
             => t -> Circle u -> Composite u
strokeCircle t  = drawCircle (cstroke t)

fillCircle :: (Floating u, Fractional u, Ord u, Fill t) 
             => t -> Circle u -> Composite u
fillCircle t  = drawCircle (fill t)

