{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.Circle
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

module Wumpus.Shapes.Circle
  ( 

    Circle(..)
  , DCircle
  , circle
  , circle_ 

  ) where


import Wumpus.Shapes.Base
import Wumpus.Shapes.Utils

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Anchors             -- package: wumpus-basic
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing

--------------------------------------------------------------------------------

-- | Coordinates

data Circle u = Circle
      { circ_radius     :: u
      , circ_ctm        :: CTM u 
      , circ_label      :: Maybe ShapeLabel
      }

type DCircle = Circle Double

type instance DUnit (Circle u) = u




-- ctr * CTM * radius      
--
withGeom :: Num u => (CTM u -> u -> a) -> Circle u -> a
withGeom f (Circle { circ_ctm=ctm, circ_radius=r }) = f ctm r
     
calcPoint :: (Real u, Floating u) => (u -> Vec2 u) -> Circle u -> Point2 u
calcPoint f = withGeom $ \ctm r -> 
    let (V2 x y) = f r in ctmDisplace x y ctm


-------------------------------------------------------------------------------- 

-- Instances 

instance (Real u, Floating u) => CenterAnchor (Circle u) where
  center = ctmCenter . circ_ctm


instance (Real u, Floating u) => CardinalAnchor (Circle u) where
  north = calcPoint $ \ r -> vvec r
  south = calcPoint $ \ r -> vvec (-r)
  east  = calcPoint $ \ r -> hvec r
  west  = calcPoint $ \ r -> hvec (-r)

instance (Real u, Floating u) => CardinalAnchor2 (Circle u) where
  northeast = calcPoint $ \ r -> avec (0.25*pi) r
  southeast = calcPoint $ \ r -> avec (1.75*pi) r
  southwest = calcPoint $ \ r -> avec (0.75*pi) r
  northwest = calcPoint $ \ r -> avec (1.25*pi) r


-- helper
updateCTM :: (CTM u -> CTM u) -> Circle u -> Circle u
updateCTM f = star (\s m -> s { circ_ctm = f m } ) circ_ctm

instance (Real u, Floating u) => Rotate (Circle u) where
  rotate r = updateCTM (rotateCTM r)

instance Num u => Scale (Circle u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Circle u) where
  translate x y = updateCTM (translateCTM x y)




--------------------------------------------------------------------------------
-- 

circle :: Num u => u -> Circle u
circle r = Circle { circ_radius = r
                  , circ_ctm    = identityCTM
                  , circ_label  = Nothing }

circle_ :: Num u => u -> String -> Circle u
circle_ r str = (circle r) { circ_label = Just $ ShapeLabel str }

--------------------------------------------------------------------------------

strokeC :: (Real u, Floating u)
        => DrawingAttr -> Point2 u -> Circle u -> Graphic u
strokeC attr (P2 x y) = 
    wrapG . cstroke (strokeAttr attr) . circlePath . translate x y 


fillC :: (Real u, Floating u) 
      => DrawingAttr -> Point2 u -> Circle u -> Graphic u
fillC attr (P2 x y) = 
    wrapG . fill (fillAttr attr) . circlePath . translate x y

textC :: (Real u, Floating u, FromPtSize u) 
      => DrawingAttr -> Point2 u -> Circle u -> Graphic u
textC attr (P2 x y) circ = maybe id sk $ circ_label circ
  where
    ctm      = circ_ctm $ translate x y circ
    sk label = labelGraphic label (textAttr attr) ctm 


make :: (Real u, Floating u) 
     => DrawingAttr -> Point2 u -> Circle u -> Circle u
make _ (P2 x y) = translate x y


instance (Real u, Floating u, FromPtSize u) => Draw (Circle u) where
  draw circ = AGraphic id df (\a p -> make a p circ)
    where
      df attr pt = textC attr pt circ . strokeC attr pt circ
                                      . fillC attr pt circ


{-

-- Hand-build the circle with bezier arcs.
--
-- This makes a better drawing than using PostScript\'s @arc@
-- command (via ellipse in wumpus-core) as line width does not
-- get affected by the CTM.
--
drawCircle :: (Real u, Floating u) 
           => (Path u -> Primitive u) -> Circle u -> Graphic u
drawCircle drawF circ = labelpic . circpic
  where
    labelpic = maybe id (labelGraphic (circ_ctm circ)) $ circ_label circ
    circpic  = wrapG $ drawF $ curvedPath $ circlePoints circ
-}



circlePath :: (Real u, Floating u) => Circle u -> Path u
circlePath = curvedPath . circlePoints 

circlePoints :: (Real u, Floating u) => Circle u -> [Point2 u]
circlePoints (Circle { circ_ctm=ctm, circ_radius=radius }) = map fn all_points
  where
    fn (P2 x y) = ctmDisplace x y ctm
    all_points  = bezierCircle 2 radius zeroPt 


{-

strokeCircle :: (Real u, Floating u, Stroke t) 
             => t -> Circle u -> Graphic u
strokeCircle t  = drawCircle (cstroke t)

fillCircle :: (Real u, Floating u, Fill t) 
             => t -> Circle u -> Graphic u
fillCircle t  = drawCircle (fill t)

-}