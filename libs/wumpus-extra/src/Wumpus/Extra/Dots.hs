{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Dots
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Dots
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Dots
  ( 

  -- * Dots
    dotHLine
  , dotVLine

  , dotX
  , dotPlus
  , dotCross
  , dotDiamond
  , dotDisk
  , dotSquare
  , dotCircle
  , dotStar
  , dotAsterisk
  , dotOPlus
  , dotOCross

  -- * Alternative
  , LineWidth
  , Dot
  , squareDot

  ) where



import Wumpus.Core
import Wumpus.Geometry

import Data.AffineSpace


-- NOTES
-- dotX -- colour, linewidth, ...
-- It makes sense to supply the point that represents the center, 
-- then we aren't having to do extra affine transformations 
-- transmitted to PostScript as (gsave, concat, ..., grestore).
--
-- Also affine transforming Points, LineSegments etc. before
-- they become pictures is _GOOD_! The calculations are done in 
-- Wumpus and so don't cause extra (gsave... grestore) in 
-- PostScript.
--
-- The size of dots is an open issue - potentially they might be 
-- sized relative to line width in the \'graphics state\' - but we 
-- have no graphics state during construction...
--
-- So maybe:
--
-- > type Dot = LineWidth u -> Picture u
--
-- Shapes seem pleasing enough where the size (height or width) 
-- is 4x the line width.
--
-- ------- -------
-- 
-- Dots should probably have their own Attribute typeclass
-- like Stroke Fill with a restricted set of options
-- (to prevent stroked dots for example).
--



dotHLine :: (Fractional u, Ord u, Stroke t)
         => t -> u -> Point2 u -> Picture u 
dotHLine attr lw pt = frame $ ostroke attr $ vertexPath [p1,p2]
  where
    p1 = pt .-^ hvec (lw*5)
    p2 = pt .+^ hvec (lw*5)


dotVLine :: (Fractional u, Ord u, Stroke t)
         => t -> u -> Point2 u -> Picture u 
dotVLine attr lw pt = frame $ ostroke attr $ vertexPath [p1,p2]
  where
    p1 = pt .-^ vvec (lw*5)
    p2 = pt .+^ vvec (lw*5)


dotX :: (Ord u, Floating u, Real u, Fractional u, Stroke t) 
     => t -> u -> Point2 u -> Picture u
dotX attr lw pt = frameMulti $ map mkStroke [ls1, ls2]
  where
    mkStroke = ostroke attr . lineSegmentToPath 
    ls1      = rotateAbout (pi/6) pt $ vlineSegmentBisect (lw*5) pt
    ls2      = reflectYPlane pt ls1  -- wrong


dotPlus :: (Ord u, Floating u, Real u, Fractional u, Stroke t) 
        => t -> u -> Point2 u -> Picture u
dotPlus attr lw pt = frameMulti $ map mkStroke [ls1, ls2]
  where
    mkStroke = ostroke attr . lineSegmentToPath 
    ls1   = vlineSegmentBisect (lw*5) pt
    ls2   = hlineSegmentBisect (lw*5) pt

dotCross :: (Ord u, Floating u, Real u, Fractional u, Stroke t) 
         => t -> u -> Point2 u -> Picture u
dotCross attr lw pt = rotate45About pt $ dotPlus attr lw pt


-- needs horizontal pinch...
dotDiamond :: (Floating u, Real u, Fractional u, Stroke t) 
           => t -> u -> Point2 u -> Picture u
dotDiamond attr lw pt = frame $ cstroke attr $ vertexPath [p1,p2,p3,p4]
  where
    hv = hvec $ lw*4
    vv = vvec $ lw*5
    p1 = pt .-^ vv
    p2 = pt .+^ hv
    p3 = pt .+^ vv
    p4 = pt .-^ hv 



dotDisk :: (Fractional u, Ord u, Ellipse t) => t -> Point2 u -> Picture u
dotDisk attr pt = frame $ ellipse attr pt 2 2


dotSquare :: (Fractional u, Ord u, Stroke t) 
          => t -> u -> Point2 u -> Picture u
dotSquare attr lw pt = 
    frame $ strokePolygon attr $ square (lw*10) (pt .-^ V2 (lw*5) (lw*5))


dotCircle :: (Floating u, Ord u, Stroke t)
          => t -> u -> Point2 u -> Picture u
dotCircle attr lw pt = frame $ cstroke attr $ circlePath 1 (lw*5) pt

-- | Five points
dotStar :: (Floating u, Real u, Ord u, Stroke t) 
          => t -> u -> Point2 u -> Picture u
dotStar attr lw pt = frameMulti $ map (ostroke attr . vertexPath . line2) xs
  where
    line2 p = [pt,p]
    xs      = circularAbout 5 (lw*5) pt

-- | Six points
dotAsterisk :: (Floating u, Real u, Stroke t)
            => t -> u -> Point2 u -> Picture u
dotAsterisk attr lw pt = 
    frameMulti $ map (ostroke attr . lineSegmentToPath) [ls1,ls2,ls3]
  where
    ls1 = vlineSegmentBisect (lw*5) pt
    ls2 = rotateAbout (pi/3) pt ls1
    ls3 = rotateAbout (pi/3) pt ls2

dotOPlus :: (Floating u, Real u, Ord u, Stroke t)
         => t -> u -> Point2 u -> Picture u
dotOPlus attr lw pt = multi [dotCircle attr lw pt, dotPlus attr lw pt]


dotOCross :: (Floating u, Real u, Ord u, Stroke t)
          => t -> u -> Point2 u -> Picture u
dotOCross attr lw pt = multi [dotCircle attr lw pt, dotCross attr lw pt]

--------------------------------------------------------------------------------
-- Alternative

-- Dots as functions extracting LineWidth from the /environment/.
-- 

type LineWidth u = u

type Dot u = LineWidth u -> Picture u


squareDot :: (Num u, Ord u, Fractional u, Stroke t) => t -> Point2 u -> Dot u
squareDot t p = frame . strokePolygon t . mkSquare where
  mkSquare lw = square (4*lw) (p .-^ (V2 (2*lw) (2*lw)))

-- Alternative no.2
--
-- LineWidth could be a type class extracting from the 
-- /fill-style/:
--
-- > squareDot :: (Num u, Ord u, Fill t, LineWidth t) 
-- >           => t -> Point2 u -> Picture u
--
