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
-- Simple shapes - rectangle, circle diamond, ellipse.
-- 
-- \*\* WARNING \*\* - the types of Shapes and Plaintext are not
-- ideal and are pending revision.
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
import Data.VectorSpace

import Control.Applicative


-- Note - Specific shapes - Rectangle, Circle, etc. - should _NOT_
-- have affine instances. 
--
-- Transformations should only operate on the Shape type. Once a
-- Shape has been drawn the resultant Rectangle, Circle... cannot 
-- be further transformed, as this would dis-associate the Anchors
-- from the Graphic.
--

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


runRectangle :: (u -> u -> ShapeGeom u a) -> Rectangle u -> a
runRectangle mf (Rectangle { rect_ctm = ctm, rect_hw = hw, rect_hh = hh }) = 
   runShapeGeom ctm $ mf hw hh 

instance (Real u, Floating u) => CenterAnchor (Rectangle u) where
  center = runRectangle (\ _ _ -> shapeCenter)


instance (Real u, Floating u) => CardinalAnchor (Rectangle u) where
  north = runRectangle $ \_  hh -> projectPoint $ P2 0 hh
  south = runRectangle $ \_  hh -> projectPoint $ P2 0 (-hh)
  east  = runRectangle $ \hw _  -> projectPoint $ P2 hw 0
  west  = runRectangle $ \hw _  -> projectPoint $ P2 (-hw) 0

instance (Real u, Floating u) => CardinalAnchor2 (Rectangle u) where
  northeast = runRectangle $ \hw hh -> projectPoint $ P2 hw hh
  southeast = runRectangle $ \hw hh -> projectPoint $ P2 hw (-hh)
  southwest = runRectangle $ \hw hh -> projectPoint $ P2 (-hw) (-hh)
  northwest = runRectangle $ \hw hh -> projectPoint $ P2 (-hw) hh


instance (Real u, Floating u) => RadialAnchor (Rectangle u) where
  radialAnchor theta = runRectangle $ \hw hh -> 
    projectPoint $ rectangleIntersect hw hh theta

-- Note - the answer needs projecting with the CTM...
--
rectangleIntersect :: (Real u, Floating u) 
                   => u -> u -> Radian -> Point2 u
rectangleIntersect hw hh theta = 
    maybe zeroPt id $ findIntersect zeroPt theta $ rectangleLines zeroPt hw hh 


-- | 'rectangle'  : @ width * height -> shape @
--
rectangle :: (Real u, Floating u) => u -> u -> LocShape u Rectangle
rectangle w h = 
    makeShape (traceLinePoints . rectanglePoints (0.5*w) (0.5*h))
              (mkRectangle (0.5*w) (0.5*h))
          

-- | 'rectangle'  : @ round_length * width * height -> shape @
--
rrectangle :: (Real u, Floating u) => u -> u -> u -> LocShape u Rectangle
rrectangle round_dist w h = 
    makeShape (roundEvery round_dist . rectanglePoints (0.5*w) (0.5*h))
              (mkRectangle (0.5*w) (0.5*h))
         



mkRectangle :: u -> u -> ShapeConstructor u Rectangle
mkRectangle hw hh = \ctm -> 
    Rectangle { rect_ctm = ctm, rect_hw = hw, rect_hh = hh }



rectanglePoints :: (Real u, Floating u) => u -> u -> ShapeCTM u -> [Point2 u]
rectanglePoints hw hh ctm = runShapeGeom ctm $ mapM projectPoint [se,ne,nw,sw]
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

runCircle :: (u -> ShapeGeom u a) -> Circle u -> a
runCircle mf (Circle { circ_ctm =ctm, circ_radius = radius }) = 
   runShapeGeom ctm $ mf radius 


instance (Real u, Floating u) => CenterAnchor (Circle u) where
  center = runCircle (\_ -> shapeCenter)


instance (Real u, Floating u) => CardinalAnchor (Circle u) where
  north = runCircle $ \r -> projectPoint $ P2 0    r
  south = runCircle $ \r -> projectPoint $ P2 0  (-r)
  east  = runCircle $ \r -> projectPoint $ P2 r    0
  west  = runCircle $ \r -> projectPoint $ P2 (-r) 0


instance (Real u, Floating u) => CardinalAnchor2 (Circle u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


instance (Real u, Floating u) => RadialAnchor (Circle u) where
  radialAnchor theta = runCircle $ \r -> projectPoint $ zeroPt .+^ avec theta r



-- | 'circle'  : @ radius -> shape @
--
circle :: (Real u, Floating u) => u -> LocShape u Circle
circle radius = makeShape (traceCurvePoints . circlePoints radius)
                          (mkCircle radius)
          


mkCircle :: u -> ShapeConstructor u Circle
mkCircle radius = \ctm -> Circle { circ_ctm = ctm, circ_radius = radius }


circlePoints :: (Real u, Floating u) => u -> ShapeCTM u -> [Point2 u]
circlePoints radius ctm = runShapeGeom ctm $ mapM projectPoint all_points
  where
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



runDiamond :: (u -> u -> ShapeGeom u a) -> Diamond u -> a
runDiamond mf (Diamond { dia_ctm = ctm, dia_hw = hw, dia_hh = hh }) = 
   runShapeGeom ctm $ mf hw hh 


instance (Real u, Floating u) => CenterAnchor (Diamond u) where
  center = runDiamond (\_ _ -> shapeCenter)

instance (Real u, Floating u) => CardinalAnchor (Diamond u) where
  north = runDiamond $ \_  hh -> projectPoint $ P2 0 hh
  south = runDiamond $ \_  hh -> projectPoint $ P2 0 (-hh)
  east  = runDiamond $ \hw _  -> projectPoint $ P2 hw 0
  west  = runDiamond $ \hw _  -> projectPoint $ P2 (-hw) 0

instance (Real u, Floating u, Fractional u) => CardinalAnchor2 (Diamond u) where
  northeast x = midpoint (north x) (east x)
  southeast x = midpoint (south x) (east x)
  southwest x = midpoint (south x) (west x)
  northwest x = midpoint (north x) (west x)



instance (Real u, Floating u) => RadialAnchor (Diamond u) where
   radialAnchor = diamondIntersect

-- Utils.Intersection needs improving...


diamondIntersect :: (Real u, Floating u) 
                 => Radian -> Diamond u -> Point2 u
diamondIntersect theta = runDiamond $ \hw hh ->  
    (\ctr ctm -> let ps = diamondPoints hw hh ctm 
                 in maybe ctr id $ findIntersect ctr theta $ polygonLines ps)
      <$> shapeCenter <*> askCTM 


midpoint :: Fractional u => Point2 u -> Point2 u -> Point2 u
midpoint p1 p2 = let v = 0.5 *^ pvec p1 p2 in p1 .+^ v

-- | 'diamond'  : @ half_width * half_height -> shape @
--
-- Note - args might change to tull_width and full_height...
--
diamond :: (Real u, Floating u) => u -> u -> LocShape u Diamond
diamond hw hh = 
    makeShape (traceLinePoints . diamondPoints hw hh)
              (mkDiamond hw hh)
          

-- | 'rdiamond'  : @ round_length * half_width * half_height -> shape @
--
-- Note - args might change to full_width and full_height...
--
rdiamond :: (Real u, Floating u) => u -> u -> u -> LocShape u Diamond
rdiamond round_dist hw hh = 
    makeShape (roundEvery round_dist . diamondPoints hw hh)
              (mkDiamond hw hh)
         




mkDiamond :: (Real u, Floating u) => u -> u -> ShapeConstructor u Diamond
mkDiamond hw hh = \ctm -> Diamond { dia_ctm = ctm, dia_hw = hw, dia_hh = hh }


diamondPoints :: (Real u, Floating u) => u -> u -> ShapeCTM u -> [Point2 u]
diamondPoints hw hh ctm = runShapeGeom ctm $ mapM projectPoint [ s, e, n, w ]
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


runEllipse :: (u -> u -> ShapeGeom u a) -> Ellipse u -> a
runEllipse mf (Ellipse { ell_ctm = ctm, ell_rx = rx, ell_ry = ry }) = 
   runShapeGeom ctm $ mf rx ry 


-- | x_radius is the unit length.
--
scaleEll :: (Scale t, Fractional u, u ~ DUnit t) => u -> u -> t -> t
scaleEll rx ry = scale 1 (ry/rx) 


instance (Real u, Floating u) => CenterAnchor (Ellipse u) where
  center = runEllipse $ \_ _ -> shapeCenter


instance (Real u, Floating u) => RadialAnchor (Ellipse u) where
  radialAnchor theta = runEllipse $ \rx ry -> 
    projectPoint $ scaleEll rx ry $ zeroPt .+^ avec theta rx


instance (Real u, Floating u) => CardinalAnchor (Ellipse u) where
  north = radialAnchor (0.5*pi)
  south = radialAnchor (1.5*pi)
  east  = radialAnchor  0
  west  = radialAnchor  pi


instance (Real u, Floating u) => CardinalAnchor2 (Ellipse u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


-- | 'ellipse'  : @ x_radii * y_radii -> shape @
--
ellipse :: (Real u, Floating u) => u -> u -> LocShape u Ellipse
ellipse rx ry = 
    makeShape (traceCurvePoints . ellipsePoints rx ry)
              (mkEllipse rx ry)
          

mkEllipse :: (Real u, Floating u) => u -> u -> ShapeConstructor u Ellipse
mkEllipse rx ry = \ctm -> Ellipse { ell_ctm = ctm, ell_rx = rx, ell_ry = ry }


ellipsePoints :: (Real u, Floating u) => u -> u -> ShapeCTM u -> [Point2 u]
ellipsePoints rx ry ctm = 
    runShapeGeom ctm $ mapM (projectPoint . scaleEll rx ry) all_points
  where
    all_points =  bezierCircle 2 rx zeroPt 



