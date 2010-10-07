{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Shapes.Derived
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- 
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes.Derived
  ( 
    Rectangle
  , DRectangle
  , rectangle
  , rrectangle

  , mkRectangle   -- hidden in Shim module

  , Circle
  , DCircle
  , circle


  , Diamond
  , DDiamond
  , diamond
  , rdiamond


  , Ellipse
  , DEllipse
  , ellipse


  ) where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Paths
import Wumpus.Basic.Paths.RoundCorners
import Wumpus.Basic.Shapes.Base
import Wumpus.Basic.Utils.Intersection

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space 


remapPoints :: (Real u, Floating u) => [Point2 u] -> ShapeCTM u -> [Point2 u]
remapPoints xs ctm = map (ctmDisplace `flip` ctm) xs


--------------------------------------------------------------------------------
-- Rectangle

data Rectangle u = Rectangle 
      { rect_ctm    :: ShapeCTM u
      , rect_hw     :: !u
      , rect_hh     :: !u 
      }
  deriving (Eq,Ord,Show)

type DRectangle = Rectangle Double

type instance DUnit (Rectangle u) = u


instance (Real u, Floating u) => CenterAnchor (Rectangle u) where
  center = ctmCenter . rect_ctm


calcRectPoint :: (Real u, Floating u) 
              => (u -> u -> Point2 u) -> Rectangle u -> Point2 u
calcRectPoint f (Rectangle { rect_ctm = ctm, rect_hw = hw, rect_hh = hh }) =
    let pt = f hw hh in ctmDisplace pt ctm

instance (Real u, Floating u) => CardinalAnchor (Rectangle u) where
  north = calcRectPoint $ \ _  hh -> P2 0 hh
  south = calcRectPoint $ \ _  hh -> P2 0 (-hh)
  east  = calcRectPoint $ \ hw _  -> P2 hw 0
  west  = calcRectPoint $ \ hw _  -> P2 (-hw) 0

instance (Real u, Floating u) => CardinalAnchor2 (Rectangle u) where
  northeast = calcRectPoint $ \ hw hh -> P2 hw hh
  southeast = calcRectPoint $ \ hw hh -> P2 hw (-hh)
  southwest = calcRectPoint $ \ hw hh -> P2 (-hw) (-hh)
  northwest = calcRectPoint $ \ hw hh -> P2 (-hw) hh


instance (Real u, Floating u) => RadialAnchor (Rectangle u) where
  radialAnchor theta rect@(Rectangle { rect_hw=hw, rect_hh=hh }) = 
      maybe ctr id $ findIntersect ctr theta $ rectangleLines ctr hw hh 
    where 
      ctr = ctmCenter $ rect_ctm rect

rectangle :: (Real u, Floating u) => u -> u -> Shape u Rectangle
rectangle w h = 
    Shape { src_ctm   = identityCTM
          , path_fun  = tracePoints . rectanglePoints (0.5*w) (0.5*h)
          , cons_fun  = mkRectangle (0.5*w) (0.5*h)  
          }

rrectangle :: (Real u, Floating u) => u -> u -> u -> Shape u Rectangle
rrectangle round_dist w h = 
    Shape { src_ctm   = identityCTM
          , path_fun  = roundEvery round_dist . rectanglePoints (0.5*w) (0.5*h)
          , cons_fun  = mkRectangle (0.5*w) (0.5*h)  
          }



mkRectangle :: u -> u -> ShapeConstructor u Rectangle
mkRectangle hw hh = \ctm -> 
    Rectangle { rect_ctm = ctm, rect_hw = hw, rect_hh = hh }



-- Note - the Paths modules should define a function for building
-- rectangles (and polygons, bezier curves / ellipses ...)
--
rectanglePoints :: (Real u, Floating u) => u -> u -> ShapeCTM u -> [Point2 u]
rectanglePoints hw hh = remapPoints [ se, ne, nw, sw ]
  where
    se = P2   hw  (-hh)
    ne = P2   hw    hh
    nw = P2 (-hw)   hh
    sw = P2 (-hw) (-hh) 





--------------------------------------------------------------------------------
-- Circle

data Circle u = Circle 
      { circ_ctm    :: ShapeCTM u
      , circ_radius :: !u 
      }
  deriving (Eq,Show)
  
type DCircle = Circle Double

type instance DUnit (Circle u) = u

instance (Real u, Floating u) => CenterAnchor (Circle u) where
  center = ctmCenter . circ_ctm


calcCircPoint :: (Real u, Floating u) 
              => (u -> Point2 u) -> Circle u -> Point2 u
calcCircPoint f (Circle { circ_ctm = ctm, circ_radius = rad }) =
    let pt = f rad in ctmDisplace pt ctm

instance (Real u, Floating u) => CardinalAnchor (Circle u) where
  north = calcCircPoint $ \ r -> P2 0  r
  south = calcCircPoint $ \ r -> P2 0 (-r)
  east  = calcCircPoint $ \ r -> P2 r  0
  west  = calcCircPoint $ \ r -> P2 (-r) 0

instance (Real u, Floating u) => RadialAnchor (Circle u) where
  radialAnchor theta = calcCircPoint $ \r -> zeroPt .+^ avec theta r


instance (Real u, Floating u) => CardinalAnchor2 (Circle u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)

circle :: (Real u, Floating u) => u -> Shape u Circle
circle radius = 
    Shape { src_ctm  = identityCTM
          , path_fun = tracePointsCurve . circlePoints radius
          , cons_fun = mkCircle radius
          }


mkCircle :: u -> ShapeConstructor u Circle
mkCircle radius = \ctm -> Circle { circ_ctm = ctm, circ_radius = radius }


circlePoints :: (Real u, Floating u) => u -> ShapeCTM u -> [Point2 u]
circlePoints radius ctm = map fn all_points
  where
    fn pt       = ctmDisplace pt ctm
    all_points  = bezierCircle 2 radius zeroPt 



--------------------------------------------------------------------------------
-- Diamond


data Diamond u = Diamond 
      { dia_ctm   :: ShapeCTM u
      , dia_hw    :: !u
      , dia_hh    :: !u
      }

type DDiamond = Diamond Double

type instance DUnit (Diamond u) = u


instance (Real u, Floating u) => CenterAnchor (Diamond u) where
  center = ctmCenter . dia_ctm


calcDiaPoint :: (Real u, Floating u) 
             => (u -> u -> Point2 u) -> Diamond u -> Point2 u
calcDiaPoint f (Diamond { dia_ctm = ctm, dia_hw = hw, dia_hh = hh }) =
    let pt = f hw hh in ctmDisplace pt ctm

instance (Real u, Floating u) => CardinalAnchor (Diamond u) where
  north = calcDiaPoint $ \ _  hh -> P2 0 hh
  south = calcDiaPoint $ \ _  hh -> P2 0 (-hh)
  east  = calcDiaPoint $ \ hw _  -> P2 hw 0
  west  = calcDiaPoint $ \ hw _  -> P2 (-hw) 0

-- | TODO - params are hw hw, should they be w h?
--
diamond :: (Real u, Floating u) => u -> u -> Shape u Diamond
diamond hw hh = 
    Shape { src_ctm  = identityCTM
          , path_fun = tracePoints . diamondPoints hw hh
          , cons_fun = mkDiamond hw hh
          }

-- | TODO - params are hw hw, should they be w h?
--
rdiamond :: (Real u, Floating u) => u -> u -> u -> Shape u Diamond
rdiamond round_dist hw hh = 
    Shape { src_ctm  = identityCTM
          , path_fun = roundEvery round_dist . diamondPoints hw hh
          , cons_fun = mkDiamond hw hh
          }




mkDiamond :: (Real u, Floating u) => u -> u -> ShapeConstructor u Diamond
mkDiamond hw hh = \ctm -> Diamond { dia_ctm = ctm, dia_hw = hw, dia_hh = hh }


diamondPoints :: (Real u, Floating u) => u -> u -> ShapeCTM u -> [Point2 u]
diamondPoints hw hh = remapPoints [ s, e, n, w ]
  where
    s = P2   0  (-hh)
    e = P2   hw    0
    n = P2   0    hh
    w = P2 (-hw)   0 


--------------------------------------------------------------------------------
-- Ellipse


data Ellipse u = Ellipse
      { ell_ctm     :: ShapeCTM u 
      , ell_rx      :: !u
      , ell_ry      :: !u
      }

type DEllipse = Ellipse Double

type instance DUnit (Ellipse u) = u



instance (Real u, Floating u) => CenterAnchor (Ellipse u) where
  center = ctmCenter . ell_ctm


ellipse :: (Real u, Floating u) => u -> u -> Shape u Ellipse
ellipse rx ry = 
    Shape { src_ctm  = identityCTM
          , path_fun = tracePointsCurve . ellipsePoints rx ry
          , cons_fun = mkEllipse rx ry  
          }



mkEllipse :: (Real u, Floating u) => u -> u -> ShapeConstructor u Ellipse
mkEllipse rx ry = \ctm -> Ellipse { ell_ctm = ctm, ell_rx = rx, ell_ry = ry }

ellipsePoints :: (Real u, Floating u) => u -> u -> ShapeCTM u -> [Point2 u]
ellipsePoints rx ry ctm = 
    map ((ctmDisplace `flip` ctm) . scaleEll rx ry) $ bezierCircle 2 rx zeroPt 


-- | x_radius is the unit length.
--
scaleEll :: (Scale t, Fractional u, u ~ DUnit t) => u -> u -> t -> t
scaleEll rx ry = scale 1 (ry/rx) 


