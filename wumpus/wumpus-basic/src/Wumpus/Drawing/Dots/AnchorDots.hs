{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Dots.AnchorDots
-- Copyright   :  (c) Stephen Tetley 2010
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

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Text.LRText
import Wumpus.Drawing.Dots.Marks

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

data Cardinal = NN | NE | EE | SE | SS | SW | WW | NW
  deriving (Eq,Show) 

type instance DUnit (DotAnchor u) = u

instance CenterAnchor (DotAnchor u) where
  center (DotAnchor ca _ _) = ca

instance RadialAnchor (DotAnchor u) where
   radialAnchor theta (DotAnchor _ ra _) = ra theta

instance CardinalAnchor (DotAnchor u) where
   north (DotAnchor _ _ c1) = c1 NN
   south (DotAnchor _ _ c1) = c1 SS
   east  (DotAnchor _ _ c1) = c1 EE
   west  (DotAnchor _ _ c1) = c1 WW



instance CardinalAnchor2 (DotAnchor u) where
   northeast (DotAnchor _ _ c1) = c1 NE
   southeast (DotAnchor _ _ c1) = c1 SE
   southwest (DotAnchor _ _ c1) = c1 SW
   northwest (DotAnchor _ _ c1) = c1 NW


radialCardinal :: Floating u => u -> Point2 u -> Cardinal -> Point2 u
radialCardinal rad ctr NN = ctr .+^ (avec (pi/2)     rad) 
radialCardinal rad ctr NE = ctr .+^ (avec (pi/4)     rad) 
radialCardinal rad ctr EE = ctr .+^ (avec  0         rad) 
radialCardinal rad ctr SE = ctr .+^ (avec (7/4 * pi) rad) 
radialCardinal rad ctr SS = ctr .+^ (avec (6/4 * pi) rad) 
radialCardinal rad ctr SW = ctr .+^ (avec (5/4 * pi) rad) 
radialCardinal rad ctr WW = ctr .+^ (avec  pi        rad) 
radialCardinal rad ctr NW = ctr .+^ (avec (3/4 * pi) rad) 


-- Rectangle cardinal points are at \"middles and corners\".
--

rectCardinal :: Floating u => u ->  u -> Point2 u -> Cardinal -> Point2 u
rectCardinal _  hh ctr NN = ctr .+^ (vvec hh) 
rectCardinal hw hh ctr NE = ctr .+^ (vec  hw     hh) 
rectCardinal hw _  ctr EE = ctr .+^ (hvec hw) 
rectCardinal hw hh ctr SE = ctr .+^ (vec  hw    (-hh)) 
rectCardinal _  hh ctr SS = ctr .+^ (vvec (-hh)) 
rectCardinal hw hh ctr SW = ctr .+^ (vec  (-hw) (-hh) )
rectCardinal hw _  ctr WW = ctr .+^ (hvec (-hw)) 
rectCardinal hw hh ctr NW = ctr .+^ (vec  (-hw)  hh) 

polyCardinal :: Floating u => (Radian -> Point2 u) -> Cardinal -> Point2 u
polyCardinal f NN = f (0.5  * pi)
polyCardinal f NE = f (0.25 * pi) 
polyCardinal f EE = f 0 
polyCardinal f SE = f (1.75 * pi) 
polyCardinal f SS = f (1.5  * pi) 
polyCardinal f SW = f (1.25 * pi)
polyCardinal f WW = f pi 
polyCardinal f NW = f (0.75 * pi) 



rectangleAnchor :: (Real u, Floating u) => u -> u -> Point2 u -> DotAnchor u
rectangleAnchor hw hh ctr = 
    DotAnchor { center_anchor   = ctr
              , radial_anchor   = fn  
              , cardinal_anchor = rectCardinal hw hh ctr }
  where
    fn theta =  maybe ctr id $ findIntersect ctr theta 
                             $ rectangleLines ctr hw hh


polygonAnchor :: (Real u, Floating u) => [Point2 u] -> Point2 u -> DotAnchor u
polygonAnchor ps ctr = 
    DotAnchor { center_anchor   = ctr
              , radial_anchor   = fn  
              , cardinal_anchor = polyCardinal fn }
  where
    fn theta =  maybe ctr id $ findIntersect ctr theta $ polygonLines ps



bboxRectAnchor  :: (Real u, Floating u) => BoundingBox u -> DotAnchor u
bboxRectAnchor (BBox bl@(P2 x1 y1) (P2 x2 y2)) =
   let hw = 0.5 * (x2 - x1)
       hh = 0.5 * (y2 - y1)
   in rectangleAnchor hw hh (bl .+^ vec hw hh)

rectangleLDO :: (Real u, Floating u) 
             => u -> u -> LocDrawingInfo u (DotAnchor u)
rectangleLDO w h = pure $ rectangleAnchor (w*0.5) (h*0.5)


circleAnchor :: Floating u => u -> Point2 u -> DotAnchor u
circleAnchor rad ctr = DotAnchor ctr 
                                 (\theta -> ctr .+^ (avec theta rad))
                                 (radialCardinal rad ctr)

circleLDO :: (Floating u, FromPtSize u) => LocDrawingInfo u (DotAnchor u)
circleLDO = bind1 (static1 markHeight) $ \diam -> 
    pure $ circleAnchor (diam * 0.5)


-- This might be better taking a function: ctr -> poly_points
-- ...
--
polygonLDO :: (Real u, Floating u, FromPtSize u) 
           => (u -> Point2 u -> [Point2 u]) -> LocDrawingInfo u (DotAnchor u)
polygonLDO mk = 
    bind1 (static1 markHeight) $ \h -> 
    promote1 $ \ctr -> let ps = mk h ctr in pure $ polygonAnchor ps ctr


--------------------------------------------------------------------------------

type DotLocImage u = LocImage u (DotAnchor u) 

type DDotLocImage = DotLocImage Double 

dotChar :: (Floating u, Real u, FromPtSize u) 
        => Char -> DotLocImage u
dotChar ch = dotText [ch]


-- | Note - dots now use font metrics...
--
dotText :: (Floating u, Real u, FromPtSize u) 
        => String -> DotLocImage u 
dotText ss = locImageMapL bboxRectAnchor (singleLineCC ss)


-- Is this generally useful? ...
--
locImageMapL :: (a -> b) -> LocImage u a -> LocImage u b
locImageMapL f = postpro1 $ \(a,prim) -> (f a, prim)



dotHLine :: (Floating u, FromPtSize u) => DotLocImage u
dotHLine = intoLocImage circleLDO markHLine


dotVLine :: (Floating u, FromPtSize u) => DotLocImage u
dotVLine = intoLocImage circleLDO markVLine


dotX :: (Floating u, FromPtSize u) => DotLocImage u
dotX = intoLocImage circleLDO markX

dotPlus :: (Floating u, FromPtSize u) => DotLocImage u
dotPlus = intoLocImage circleLDO markPlus

dotCross :: (Floating u, FromPtSize u) => DotLocImage u
dotCross = intoLocImage circleLDO markCross

dotDiamond :: (Floating u, FromPtSize u) => DotLocImage u
dotDiamond = intoLocImage circleLDO markDiamond

dotFDiamond :: (Floating u, FromPtSize u) => DotLocImage u
dotFDiamond = intoLocImage circleLDO markFDiamond



dotDisk :: (Floating u, FromPtSize u) => DotLocImage u
dotDisk = intoLocImage circleLDO markDisk


dotSquare :: (Floating u, Real u, FromPtSize u) => DotLocImage u
dotSquare = bind1 (static1 markHeight) $ \ h ->
    intoLocImage (rectangleLDO h h) markSquare




dotCircle :: (Floating u, FromPtSize u) => DotLocImage u
dotCircle = intoLocImage circleLDO markCircle


dotPentagon :: (Floating u, FromPtSize u) => DotLocImage u
dotPentagon = intoLocImage circleLDO markPentagon

dotStar :: (Floating u, FromPtSize u) => DotLocImage u
dotStar = intoLocImage circleLDO markStar


dotAsterisk :: (Floating u, FromPtSize u) => DotLocImage u
dotAsterisk = intoLocImage circleLDO markAsterisk

dotOPlus :: (Floating u, FromPtSize u) => DotLocImage u
dotOPlus = intoLocImage circleLDO markOPlus

dotOCross :: (Floating u, FromPtSize u) => DotLocImage u
dotOCross = intoLocImage circleLDO markOCross

dotFOCross :: (Floating u, FromPtSize u) => DotLocImage u
dotFOCross = intoLocImage circleLDO markFOCross


dotTriangle :: (Real u, Floating u, FromPtSize u) => DotLocImage u
dotTriangle = intoLocImage (polygonLDO fn) markTriangle
  where 
    fn h ctr = let (bl,br,top) = equilateralTrianglePoints h ctr in [bl,br,top]
