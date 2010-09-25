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
  , lrectangle

  , Circle
  , DCircle
  , circle
  , lcircle

  , Coordinate
  , DCoordinate
  , coordinate

  , Diamond
  , DDiamond
  , diamond
  , ldiamond

  , Ellipse
  , DEllipse
  , ellipse
  , lellipse

  ) where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Graphic
import Wumpus.Basic.Shapes.Base


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space 

import Data.Monoid


--------------------------------------------------------------------------------
-- Rectangle

data Rectangle u = Rectangle 
      { rect_ctm    :: ShapeCTM u
      , rect_hw     :: !u
      , rect_hh     :: !u 
      }
  deriving (Eq,Show)

type DRectangle = Rectangle Double

-- Not deriving much value from Shape...

-- type SRectangle u = SRectangle { getRectangle :: Shape Rectangle u }

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


rectangle :: (Floating u, Real u) => u -> u -> Shape u (Rectangle u)
rectangle hw hh = Shape { src_ctm = identityCTM
                        , out_fun = outputRect hw hh nolabel
                        }


lrectangle :: (Floating u, Real u, FromPtSize u) 
           => u -> u -> String -> Shape u (Rectangle u)
lrectangle hw hh ss = Shape { src_ctm = identityCTM
                            , out_fun = outputRect hw hh (shapelabel ss)
                            }


outputRect :: (Real u, Floating u) 
           => u -> u -> ShapeLabel u -> ShapeCTM u -> Image u (Rectangle u)
outputRect hw hh shl ctm = intoImage (pureDF a) (drawRect a `mappend` label) 
  where
    a     = Rectangle { rect_ctm = ctm, rect_hw = hw, rect_hh = hh }
    label = runShapeLabel ctm shl


drawRect :: (Real u, Floating u) => Rectangle u -> Graphic u
drawRect = borderedPath . rectPath


rectPath :: (Real u, Floating u) => Rectangle u -> PrimPath u
rectPath rect = vertexPath [ southwest rect
                           , southeast rect
                           , northeast rect
                           , northwest rect
                           ]





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
  southwest = radialAnchor (0.75*pi)
  northwest = radialAnchor (1.25*pi)


circle :: (Floating u, Real u) => u -> Shape u (Circle u)
circle radius = Shape { src_ctm = identityCTM
                      , out_fun = outputCirc radius nolabel
                      }


lcircle :: (Floating u, Real u, FromPtSize u) 
        => u -> String -> Shape u (Circle u)
lcircle radius ss = Shape { src_ctm = identityCTM
                          , out_fun = outputCirc radius (shapelabel ss)
                          }

outputCirc :: (Real u, Floating u) 
           => u -> ShapeLabel u -> ShapeCTM u -> Image u (Circle u)
outputCirc rad shl ctm = intoImage (pureDF a) (drawCirc a `mappend` label) 
  where
    a     = Circle { circ_ctm = ctm, circ_radius = rad }
    label = runShapeLabel ctm shl


drawCirc :: (Real u, Floating u) => Circle u -> Graphic u
drawCirc = borderedPath . circlePath


circlePath :: (Real u, Floating u) => Circle u -> PrimPath u
circlePath = curvedPath . circlePoints 

circlePoints :: (Real u, Floating u) => Circle u -> [Point2 u]
circlePoints (Circle { circ_ctm=ctm, circ_radius=radius }) = map fn all_points
  where
    fn pt       = ctmDisplace pt ctm
    all_points  = bezierCircle 2 radius zeroPt 


--------------------------------------------------------------------------------
-- | Coordinate

data Coordinate u = Coordinate
      { coord_ctm   :: ShapeCTM u 
      }

type DCoordinate = Coordinate Double

type instance DUnit (Coordinate u) = u

instance (Real u, Floating u) => CenterAnchor (Coordinate u) where
  center = ctmCenter . coord_ctm


coordinate :: (Real u, Floating u) => Shape u (Coordinate u)
coordinate = Shape { src_ctm = identityCTM
                   , out_fun = outputCoord
                   }

outputCoord :: (Real u, Floating u) => ShapeCTM u -> Image u (Coordinate u)
outputCoord ctm = intoImage (pureDF a) (drawCoord a) 
  where
    a = Coordinate { coord_ctm = ctm }


drawCoord :: (Real u, Floating u) => Coordinate u -> Graphic u
drawCoord coord = localDF swapColours $ filledEllipse 2 2 (center coord)

--------------------------------------------------------------------------------
-- Diamond


data Diamond u = Diamond 
      { dia_ctm   :: ShapeCTM u
      , dia_hw    :: u
      , dia_hh    :: u
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

diamond :: (Floating u, Real u) => u -> u -> Shape u (Diamond u)
diamond hw hh = Shape { src_ctm = identityCTM
                      , out_fun = outputDia hw hh nolabel
                      }


ldiamond :: (Floating u, Real u, FromPtSize u) 
         => u -> u -> String -> Shape u (Diamond u)
ldiamond hw hh ss = Shape { src_ctm = identityCTM
                          , out_fun = outputDia hw hh (shapelabel ss)
                          }



outputDia :: (Real u, Floating u) 
          => u -> u -> ShapeLabel u -> ShapeCTM u -> Image u (Diamond u)
outputDia hw hh shl ctm = intoImage (pureDF a) (drawDia a `mappend` label) 
  where
    a     = Diamond { dia_ctm = ctm, dia_hw = hw, dia_hh = hh }
    label = runShapeLabel ctm shl

drawDia :: (Real u, Floating u) => Diamond u -> Graphic u
drawDia = borderedPath . diamondPath


diamondPath :: (Real u, Floating u) => Diamond u -> PrimPath u
diamondPath dia = vertexPath [ south dia, east dia, north dia, west dia ]


--------------------------------------------------------------------------------
-- Ellipse


data Ellipse u = Ellipse
      { ell_ctm     :: ShapeCTM u 
      , ell_rx      :: u
      , ell_ry      :: u
      }

type DEllipse = Ellipse Double

type instance DUnit (Ellipse u) = u



instance (Real u, Floating u) => CenterAnchor (Ellipse u) where
  center = ctmCenter . ell_ctm


ellipse :: (Real u, Floating u) => u -> u -> Shape u (Ellipse u)
ellipse rx ry = Shape { src_ctm = identityCTM
                      , out_fun = outputEll rx ry nolabel
                      }

lellipse :: (Real u, Floating u, FromPtSize u) 
         => u -> u -> String -> Shape u (Ellipse u)
lellipse rx ry ss = Shape { src_ctm = identityCTM
                          , out_fun = outputEll rx ry (shapelabel ss)
                          }


outputEll :: (Real u, Floating u) 
          => u -> u -> ShapeLabel u -> ShapeCTM u -> Image u (Ellipse u)
outputEll rx ry shl ctm = intoImage (pureDF a) (drawEll a `mappend` label)
  where
    a     = Ellipse { ell_ctm = ctm, ell_rx = rx, ell_ry = ry }
    label = runShapeLabel ctm shl


drawEll :: (Real u, Floating u) => Ellipse u -> Graphic u
drawEll = borderedPath . ellipsePath


ellipsePath :: (Real u, Floating u) => Ellipse u -> PrimPath u
ellipsePath = curvedPath . ellipsePoints

ellipsePoints :: (Real u, Floating u) => Ellipse u -> [Point2 u]
ellipsePoints (Ellipse { ell_ctm=ctm, ell_rx=rx, ell_ry=ry }) = 
    map (ctmDisplace `flip` ctm) all_points
  where
    all_points  = map (rescale rx ry) $ bezierCircle 2 rx zeroPt 


-- | x_radius is the unit length.
--
rescale :: (Scale t, Fractional u, u ~ DUnit t) => u -> u -> t -> t
rescale rx ry = scale 1 (ry/rx) 


