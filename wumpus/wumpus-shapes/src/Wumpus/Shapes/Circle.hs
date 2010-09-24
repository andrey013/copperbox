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

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Anchors                     -- package: wumpus-basic
import Wumpus.Basic.Graphic

import Control.Applicative
import Data.Monoid

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
updateCTM f = (\s m -> s { circ_ctm = f m } ) <*> circ_ctm

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
        => Circle u -> Graphic u
strokeC = closedStroke . circlePath

borderedC :: (Real u, Floating u) 
      => Circle u -> Graphic u
borderedC =  borderedPath . circlePath


textC :: (Real u, Floating u, FromPtSize u) 
      => Circle u -> Graphic u
textC circ = maybe mempty sk $ circ_label circ
  where
    sk label = labelGraphic label (circ_ctm circ)


instance (Real u, Floating u, FromPtSize u) => DrawShape (Circle u) where
  drawShape circ = intoImage (pureDF circ) (borderedC circ `mappend` textC circ)

instance (Real u, Floating u, FromPtSize u) => OutlineShape (Circle u) where
  outlineShape circ = intoImage (pureDF circ) (strokeC circ `mappend` textC circ)


circlePath :: (Real u, Floating u) => Circle u -> PrimPath u
circlePath = curvedPath . circlePoints 

circlePoints :: (Real u, Floating u) => Circle u -> [Point2 u]
circlePoints (Circle { circ_ctm=ctm, circ_radius=radius }) = map fn all_points
  where
    fn (P2 x y) = ctmDisplace x y ctm
    all_points  = bezierCircle 2 radius zeroPt 


