{-# LANGUAGE TypeFamilies               #-}
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
  , smallDisk
  , largeDisk

  , smallCirc
  , largeCirc


  , dotNone
  , dotChar
  , dotText
  , dotHBar
  , dotVBar
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


import Wumpus.Drawing.Dots.SimpleDots ( MarkSize )
import qualified Wumpus.Drawing.Dots.SimpleDots as SD

import Wumpus.Basic.Geometry                    -- package: wumpus-basic
import Wumpus.Basic.Kernel               

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


import Control.Applicative


-- | All dots return the same thing a 'DotAnchor' which supports 
-- the same (limited) see of anchors.
--
data DotAnchor u = DotAnchor 
      { center_anchor   :: Point2 u
      , radial_anchor   :: Radian   -> Point2 u
      , cardinal_anchor :: Cardinal -> Point2 u 
      }

type instance DUnit (DotAnchor u) = u

instance Num u => Translate (DotAnchor u) where
  translate x y (DotAnchor ctr radialF cardinalF) = 
      DotAnchor { center_anchor   = translate x y ctr
                , radial_anchor   = translate x y . radialF 
                , cardinal_anchor = translate x y . cardinalF 
                }


instance CenterAnchor (DotAnchor u) where
  center (DotAnchor ca _ _) = ca

instance RadialAnchor (DotAnchor u) where
   radialAnchor theta (DotAnchor _ ra _) = ra theta

instance CardinalAnchor (DotAnchor u) where
   north (DotAnchor _ _ c1) = c1 NORTH
   south (DotAnchor _ _ c1) = c1 SOUTH
   east  (DotAnchor _ _ c1) = c1 EAST
   west  (DotAnchor _ _ c1) = c1 WEST



instance CardinalAnchor2 (DotAnchor u) where
   northeast (DotAnchor _ _ c1) = c1 NORTH_EAST
   southeast (DotAnchor _ _ c1) = c1 SOUTH_EAST
   southwest (DotAnchor _ _ c1) = c1 SOUTH_WEST
   northwest (DotAnchor _ _ c1) = c1 NORTH_WEST


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


-- | All anchors are the center!
--
zeroAnchor :: Point2 u -> DotAnchor u
zeroAnchor ctr = 
    DotAnchor { center_anchor   = ctr
              , radial_anchor   = const ctr 
              , cardinal_anchor = const ctr }


rectangleAnchor :: (Real u, Floating u) => u -> u -> Point2 u -> DotAnchor u
rectangleAnchor hw hh ctr = 
    DotAnchor { center_anchor   = ctr
              , radial_anchor   = fn  
              , cardinal_anchor = rectCardinal hw hh ctr }
  where
    fn theta =  displace (rectRadialVector hw hh theta) ctr


circleAnchor :: Floating u => u -> Point2 u -> DotAnchor u
circleAnchor rad ctr = 
    DotAnchor { center_anchor   = ctr
              , radial_anchor   = fn 
              , cardinal_anchor = radialCardinal rad ctr }
  where
    fn theta = displace (avec theta rad) ctr


polygonAnchor :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
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


zeroLDO :: InterpretUnit u => LocQuery u (DotAnchor u)
zeroLDO = qpromoteLoc $ \pt -> return $ zeroAnchor pt

rectangleLDO :: (Real u, Floating u, InterpretUnit u) 
             => MarkSize -> MarkSize -> LocQuery u (DotAnchor u)
rectangleLDO w h = qpromoteLoc $ \pt -> 
    (\uw uh -> rectangleAnchor (uw*0.5) (uh*0.5) pt) 
      <$> uconvertCtx1 w <*> uconvertCtx1 h



circleLDO :: (Floating u, InterpretUnit u) 
          => MarkSize -> LocQuery u (DotAnchor u)
circleLDO rad = qpromoteLoc $ \pt -> 
    uconvertCtx1 rad >>= \urad ->  pure $ circleAnchor urad pt


-- Probably better just using bounding circle for polygons 
-- If you really care about anchors use shapes
-- 

-- Triangle probably benefits proper calculation...

triangleLDO :: (Real u, Floating u, Tolerance u, InterpretUnit u) 
            => MarkSize -> LocQuery u (DotAnchor u)
triangleLDO h = qpromoteLoc $ \pt -> 
    uconvertCtx1 h >>= \uh -> 
    let alg = trailIterateLocus $ fn3 $ equilateralTriangleVertices uh
    in (\ps -> polygonAnchor ps pt) 
         <$> qapplyLoc (placedTrailPoints alg) pt
  where
    fn3 (a,b,c) = [a,b,c]


--------------------------------------------------------------------------------


type DotLocImage u = LocImage u (DotAnchor u)

type DDotLocImage = DotLocImage Double 


dotNone :: InterpretUnit u => DotLocImage u
dotNone = intoLocImage zeroLDO SD.dotNone



smallDisk :: (Floating u, Real u, InterpretUnit u) => DotLocImage u
smallDisk = intoLocImage (circleLDO 0.25) SD.smallDisk


largeDisk :: (Floating u, Real u, InterpretUnit u) => DotLocImage u
largeDisk = intoLocImage (circleLDO 1.00) SD.largeDisk

smallCirc :: (Floating u, Real u, InterpretUnit u) => DotLocImage u
smallCirc = intoLocImage (circleLDO 0.25) SD.smallCirc


largeCirc :: (Floating u, Real u, InterpretUnit u) => DotLocImage u
largeCirc = intoLocImage (circleLDO 1.00) SD.largeCirc


dotChar :: (Floating u, Real u, InterpretUnit u) => Char -> DotLocImage u
dotChar ch = dotText [ch]


-- Note - dotText now uses font metrics, the generated BBox is 
-- fine for dots (if they are all the same text) but not good for 
-- tree nodes (for example). Wumpus-Tree should really be using a
-- different graphic object for labelled trees.
--


dotText :: (Floating u, Real u, InterpretUnit u) => String -> DotLocImage u 
dotText ss = 
    fmap bboxRectAnchor $ runPosObjectBBox CENTER $ posText ss

-- Note - maybe Wumpus-Basic should have a @swapAns@ function?


dotHBar :: (Floating u, InterpretUnit u) => DotLocImage u
dotHBar = intoLocImage (circleLDO 0.5) SD.dotHBar


dotVBar :: (Floating u, InterpretUnit u) => DotLocImage u
dotVBar = intoLocImage (circleLDO 0.5) SD.dotVBar


dotX :: (Floating u, InterpretUnit u) => DotLocImage u
dotX = intoLocImage (circleLDO 0.5) SD.dotX

dotPlus :: (Floating u, InterpretUnit u) => DotLocImage u
dotPlus = intoLocImage (circleLDO 0.5) SD.dotPlus

dotCross :: (Floating u, InterpretUnit u) => DotLocImage u
dotCross = intoLocImage (circleLDO 0.5) SD.dotCross

dotDiamond :: (Floating u, InterpretUnit u) => DotLocImage u
dotDiamond = intoLocImage (circleLDO 0.5) SD.dotDiamond

dotFDiamond :: (Floating u, InterpretUnit u) => DotLocImage u
dotFDiamond = intoLocImage (circleLDO 0.5) SD.dotFDiamond



dotDisk :: (Floating u, InterpretUnit u) => DotLocImage u
dotDisk = intoLocImage (circleLDO 0.5) SD.dotDisk


dotSquare :: (Floating u, Real u, InterpretUnit u) => DotLocImage u
dotSquare = intoLocImage (rectangleLDO 1 1) SD.dotSquare




dotCircle :: (Floating u, InterpretUnit u) => DotLocImage u
dotCircle = intoLocImage (circleLDO 0.5) SD.dotCircle


dotPentagon :: (Floating u, InterpretUnit u) => DotLocImage u
dotPentagon = intoLocImage (circleLDO 0.5) SD.dotPentagon

dotStar :: (Floating u, Ord u, InterpretUnit u, Tolerance u) 
        => DotLocImage u
dotStar = intoLocImage (circleLDO 0.5) SD.dotStar


dotAsterisk :: (Floating u, InterpretUnit u) => DotLocImage u
dotAsterisk = intoLocImage (circleLDO 0.5) SD.dotAsterisk

dotOPlus :: (Floating u, InterpretUnit u) => DotLocImage u
dotOPlus = intoLocImage (circleLDO 0.5) SD.dotOPlus

dotOCross :: (Floating u, InterpretUnit u) => DotLocImage u
dotOCross = intoLocImage (circleLDO 0.5) SD.dotOCross

dotFOCross :: (Floating u, InterpretUnit u) => DotLocImage u
dotFOCross = intoLocImage (circleLDO 0.5) SD.dotFOCross


dotTriangle :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
            => DotLocImage u
dotTriangle = intoLocImage (triangleLDO 1) SD.dotTriangle


intoLocImage :: InterpretUnit u 
             => LocQuery u a -> LocImage u z -> LocImage u a
intoLocImage ma gf = promoteLoc $ \pt -> 
                     askDC >>= \ctx -> 
                     let ans = runLocQuery ctx pt ma
                     in replaceAns ans $ applyLoc gf pt