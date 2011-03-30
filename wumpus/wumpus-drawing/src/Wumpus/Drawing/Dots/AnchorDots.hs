{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Dots.AnchorDots
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Dots with anchors.
--
-- In many cases a surrounding circle is used to locate anchor
-- points - this could be improved to use the actual dot border 
-- at some point.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Dots.AnchorDots
  ( 

  -- * Existential anchor type
    DotAnchor
 
  , DotLocImage
  , DDotLocImage

  -- * Dots with anchor points
  , dotChar
  , dotText
  , dotHLine
  , dotVLine
  , dotX
  , dotPlus
  , dotCross
  , dotDiamond
  , dotFDiamond

  , dotDisk
  , dotSquare
  , dotCircle
  , dotPentagon
  , dotStar

  , dotAsterisk
  , dotOPlus
  , dotOCross
  , dotFOCross

  , dotTriangle

  ) where


import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Text.Base.RotTextZero

import Wumpus.Basic.Geometry.Base               -- package: wumpus-basic
import Wumpus.Basic.Geometry.Intersection
import Wumpus.Basic.Geometry.Paths
import Wumpus.Basic.Geometry.Quadrant
import Wumpus.Basic.Kernel               

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


import Control.Applicative

-- An existential thing that supports anchors.
-- This means any dot can retun the same (opaque) structure
--
-- But it does mean that which anchor class are supported is 
-- fixed - the datatype needs a field for each one.
-- Supporting north, southeast etc. will also be tedious...
--
data DotAnchor u = forall s.  
                    DotAnchor { center_anchor   :: Point2 u
                              , radial_anchor   :: Radian   -> Point2 u
                              , cardinal_anchor :: Cardinal -> Point2 u }



instance CenterAnchor DotAnchor u where
  center (DotAnchor ca _ _) = pure ca

instance RadialAnchor DotAnchor u where
   radialAnchor theta (DotAnchor _ ra _) = pure $ ra theta

instance CardinalAnchor DotAnchor u where
   north (DotAnchor _ _ c1) = pure $ c1 NORTH
   south (DotAnchor _ _ c1) = pure $ c1 SOUTH
   east  (DotAnchor _ _ c1) = pure $ c1 EAST
   west  (DotAnchor _ _ c1) = pure $ c1 WEST



instance CardinalAnchor2 DotAnchor u where
   northeast (DotAnchor _ _ c1) = pure $ c1 NORTH_EAST
   southeast (DotAnchor _ _ c1) = pure $ c1 SOUTH_EAST
   southwest (DotAnchor _ _ c1) = pure $ c1 SOUTH_WEST
   northwest (DotAnchor _ _ c1) = pure $ c1 NORTH_WEST


radialCardinal :: Floating u => u -> Point2 u -> Cardinal -> Point2 u
radialCardinal rad ctr NORTH        = ctr .+^ (avec (pi/2)     rad) 
radialCardinal rad ctr NORTH_EAST   = ctr .+^ (avec (pi/4)     rad) 
radialCardinal rad ctr EAST         = ctr .+^ (avec  0         rad) 
radialCardinal rad ctr SOUTH_EAST   = ctr .+^ (avec (7/4 * pi) rad) 
radialCardinal rad ctr SOUTH        = ctr .+^ (avec (6/4 * pi) rad) 
radialCardinal rad ctr SOUTH_WEST   = ctr .+^ (avec (5/4 * pi) rad) 
radialCardinal rad ctr WEST         = ctr .+^ (avec  pi        rad) 
radialCardinal rad ctr NORTH_WEST   = ctr .+^ (avec (3/4 * pi) rad) 


-- Rectangle cardinal points are at \"middles and corners\".
--

rectCardinal :: Floating u => u ->  u -> Point2 u -> Cardinal -> Point2 u
rectCardinal _  hh ctr NORTH        = ctr .+^ (vvec hh) 
rectCardinal hw hh ctr NORTH_EAST   = ctr .+^ (vec  hw     hh) 
rectCardinal hw _  ctr EAST         = ctr .+^ (hvec hw) 
rectCardinal hw hh ctr SOUTH_EAST   = ctr .+^ (vec  hw    (-hh)) 
rectCardinal _  hh ctr SOUTH        = ctr .+^ (vvec (-hh)) 
rectCardinal hw hh ctr SOUTH_WEST   = ctr .+^ (vec  (-hw) (-hh) )
rectCardinal hw _  ctr WEST         = ctr .+^ (hvec (-hw)) 
rectCardinal hw hh ctr NORTH_WEST   = ctr .+^ (vec  (-hw)  hh) 

polyCardinal :: Floating u => (Radian -> Point2 u) -> Cardinal -> Point2 u
polyCardinal f NORTH                = f (0.5  * pi)
polyCardinal f NORTH_EAST           = f (0.25 * pi) 
polyCardinal f EAST                 = f 0 
polyCardinal f SOUTH_EAST           = f (1.75 * pi) 
polyCardinal f SOUTH                = f (1.5  * pi) 
polyCardinal f SOUTH_WEST           = f (1.25 * pi)
polyCardinal f WEST                 = f pi 
polyCardinal f NORTH_WEST           = f (0.75 * pi) 



rectangleAnchor :: (Real u, Floating u) => u -> u -> Point2 u -> DotAnchor u
rectangleAnchor hw hh ctr = 
    DotAnchor { center_anchor   = ctr
              , radial_anchor   = fn  
              , cardinal_anchor = rectCardinal hw hh ctr }
  where
    fn theta =  displaceVec (rectRadialVector hw hh theta) ctr


polygonAnchor :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
              => [Point2 u] -> Point2 u -> DotAnchor u
polygonAnchor ps ctr = 
    DotAnchor { center_anchor   = ctr
              , radial_anchor   = fn  
              , cardinal_anchor = polyCardinal fn }
  where
    fn theta =  maybe ctr id $ findIntersect ctr theta 
                             $ polygonLineSegments ps



bboxRectAnchor  :: (Real u, Floating u) => BoundingBox u -> DotAnchor u
bboxRectAnchor (BBox bl@(P2 x1 y1) (P2 x2 y2)) =
   let hw = 0.5 * (x2 - x1)
       hh = 0.5 * (y2 - y1)
   in rectangleAnchor hw hh (bl .+^ vec hw hh)

rectangleLDO :: (Real u, Floating u) 
             => u -> u -> LocQuery u (DotAnchor u)
rectangleLDO w h = 
    promoteR1 $ \pt -> pure $ rectangleAnchor (w*0.5) (h*0.5) pt


circleAnchor :: Floating u => u -> Point2 u -> DotAnchor u
circleAnchor rad ctr = DotAnchor ctr 
                                 (\theta -> ctr .+^ (avec theta rad))
                                 (radialCardinal rad ctr)

circleLDO :: (Floating u, InterpretUnit u) => LocQuery u (DotAnchor u)
circleLDO = 
    promoteR1 $ \pt -> 
      markHeight >>= \diam -> pure $ circleAnchor (diam * 0.5) pt


-- This might be better taking a function: ctr -> poly_points
-- ...
--
polygonLDO :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
           => (u -> Point2 u -> [Point2 u]) -> LocQuery u (DotAnchor u)
polygonLDO mk = 
    promoteR1 $ \ctr -> 
      markHeight >>= \h -> let ps = mk h ctr in pure $ polygonAnchor ps ctr


--------------------------------------------------------------------------------


type DotLocImage u = LocImage DotAnchor u

type DDotLocImage = DotLocImage Double 

dotChar :: (Floating u, Real u, InterpretUnit u) => Char -> DotLocImage u
dotChar ch = dotText [ch]


-- Note - dotText now uses font metrics, the generated BBox is 
-- fine for dots (if they are all the same text) but not good for 
-- tree nodes (for example). Wumpus-Tree should really be using a
-- different graphic object for labelled trees.
--


dotText :: (Floating u, Real u, InterpretUnit u) => String -> DotLocImage u 
dotText ss = mapAns bboxRectAnchor $ ccTextline ss

-- Note - maybe Wumpus-Basic should have a @swapAns@ function?


dotHLine :: (Floating u, InterpretUnit u) => DotLocImage u
dotHLine = intoLocImage circleLDO markHLine


dotVLine :: (Floating u, InterpretUnit u) => DotLocImage u
dotVLine = intoLocImage circleLDO markVLine


dotX :: (Floating u, InterpretUnit u) => DotLocImage u
dotX = intoLocImage circleLDO markX

dotPlus :: (Floating u, InterpretUnit u) => DotLocImage u
dotPlus = intoLocImage circleLDO markPlus

dotCross :: (Floating u, InterpretUnit u) => DotLocImage u
dotCross = intoLocImage circleLDO markCross

dotDiamond :: (Floating u, InterpretUnit u) => DotLocImage u
dotDiamond = intoLocImage circleLDO markDiamond

dotFDiamond :: (Floating u, InterpretUnit u) => DotLocImage u
dotFDiamond = intoLocImage circleLDO markFDiamond



dotDisk :: (Floating u, InterpretUnit u) => DotLocImage u
dotDisk = intoLocImage circleLDO markDisk


dotSquare :: (Floating u, Real u, InterpretUnit u) => DotLocImage u
dotSquare = 
    markHeight >>= \h -> intoLocImage (rectangleLDO h h) markSquare




dotCircle :: (Floating u, InterpretUnit u) => DotLocImage u
dotCircle = intoLocImage circleLDO markCircle


dotPentagon :: (Floating u, InterpretUnit u) => DotLocImage u
dotPentagon = intoLocImage circleLDO markPentagon

dotStar :: (Floating u, InterpretUnit u) => DotLocImage u
dotStar = intoLocImage circleLDO markStar


dotAsterisk :: (Floating u, InterpretUnit u) => DotLocImage u
dotAsterisk = intoLocImage circleLDO markAsterisk

dotOPlus :: (Floating u, InterpretUnit u) => DotLocImage u
dotOPlus = intoLocImage circleLDO markOPlus

dotOCross :: (Floating u, InterpretUnit u) => DotLocImage u
dotOCross = intoLocImage circleLDO markOCross

dotFOCross :: (Floating u, InterpretUnit u) => DotLocImage u
dotFOCross = intoLocImage circleLDO markFOCross


dotTriangle :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
            => DotLocImage u
dotTriangle = intoLocImage (polygonLDO fn) markTriangle
  where 
    fn h ctr = let (bl,br,top) = equilateralTrianglePoints h ctr in [bl,br,top]
