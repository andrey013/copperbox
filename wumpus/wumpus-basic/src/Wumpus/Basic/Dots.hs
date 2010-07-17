{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Dots
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Dots
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Dots
  ( 

  -- * Mark drawing attributes
    MarkAttr(..)
  , standardAttr   

  -- * Dots
  , dotHLine
  , dotVLine
  , dotX
  , dotPlus
  , dotCross
{-  , dotDiamond
  , dotDisk
  , dotSquare
  , dotCircle
  , dotStar
  , dotAsterisk
  , dotOPlus
  , dotOCross
-}

  ) where


import Wumpus.Basic.Graphic
import Wumpus.Basic.SVGColours
import Wumpus.Core                      -- package: wumpus-core

-- import Data.AffineSpace                 -- package: vector-space

import Control.Applicative


data MarkAttr = MarkAttr 
      { line_width         :: Double
      , point_size         :: FontSize
      , mark_colour        :: DRGB
      , mark_second_colour :: DRGB
      }
  deriving (Eq,Show)

standardAttr :: FontSize -> MarkAttr
standardAttr sz = MarkAttr { line_width         = 1.0
                           , point_size         = sz
                           , mark_colour        = black
                           , mark_second_colour = gold  }

 
primaryAttr :: MarkAttr -> (DRGB, StrokeAttr)
primaryAttr = liftA2 (,) mark_colour (LineWidth . line_width)

markHeight :: Num u => MarkAttr -> u
markHeight = fromIntegral . point_size

-- Marks should be the height of a lower-case letter...

-- NOTES
--
-- Affine transforming Points, LineSegments etc. before
-- they become pictures is _GOOD_! The calculations are done in 
-- Wumpus and so don't cause extra (gsave... grestore) in 
-- PostScript.
--


-- Better would be a version of straightLine where the point is 
-- the center not the start...
-- 
dotHLine :: Fractional u => MarkAttr -> GraphicF u 
dotHLine attr = let w = markHeight attr in 
    straightLine (primaryAttr attr) (hvec w)
    

dotVLine :: Fractional u => MarkAttr -> GraphicF u 
dotVLine attr = let h = markHeight attr in 
    straightLine (primaryAttr attr) (vvec h)


dotX :: Fractional u => MarkAttr -> GraphicF u
dotX attr = ls1 `cc` ls2
  where
    h        = markHeight attr
    w        = 0.75 * h
    ls1      = straightLine (primaryAttr attr) (vec w    h)
    ls2      = straightLine (primaryAttr attr) (vec (-w) h)


dotPlus :: Fractional u => MarkAttr -> GraphicF u
dotPlus attr = dotVLine attr `cc` dotHLine attr


dotCross :: Fractional u => MarkAttr -> GraphicF u
dotCross attr = ls1 `cc` ls2
  where
    z        = markHeight attr
    ls1      = straightLine (primaryAttr attr) (vec z    z)
    ls2      = straightLine (primaryAttr attr) (vec (-z) z)

{-
-- needs horizontal pinch...
dotDiamond :: (Floating u, Real u, Fractional u, Mark t) 
           => t -> Point2 u -> Picture u
dotDiamond attr pt = frame $ mcstroke attr $ vertexPath [p1,p2,p3,p4]
  where
    hv = hvec $ (4*) $ lineWidth attr
    vv = vvec $ lwX5 attr
    p1 = pt .-^ vv
    p2 = pt .+^ hv
    p3 = pt .+^ vv
    p4 = pt .-^ hv 



dotDisk :: (Fractional u, Floating u, Ord u, Mark t) 
        => t -> Point2 u -> Picture u
dotDisk attr pt = frame $ mellipse attr radius radius pt where 
     radius = lwX5 attr


dotSquare :: (Fractional u, Floating u, Ord u, Mark t) 
          => t -> Point2 u -> Picture u
dotSquare attr pt = 
    frame $ mstrokePolygon attr $ square (lwX10 attr) (pt .-^ vec) 
  where
    vec = dup V2 (lwX5 attr)

dotCircle :: (Floating u, Ord u, Mark t)
          => t -> Point2 u -> Picture u
dotCircle attr pt = frame $ mcstroke attr $ circlePath 1 (lwX5 attr) pt

-- | Five points
dotStar :: (Floating u, Real u, Ord u, Mark t) 
          => t -> Point2 u -> Picture u
dotStar attr pt = frameMulti $ map (mostroke attr . vertexPath . line2) xs
  where
    line2 p = [pt,p]
    xs      = circularAbout 5 (lwX5 attr) pt

-- | Six points
dotAsterisk :: (Floating u, Real u, Mark t)
            => t -> Point2 u -> Picture u
dotAsterisk attr pt = 
    frameMulti $ map (mostroke attr . lineSegmentToPath) [ls1,ls2,ls3]
  where
    ls1 = vlineSegmentMid (lwX10 attr) pt
    ls2 = rotateAbout (pi/3) pt ls1
    ls3 = rotateAbout (pi/3) pt ls2

dotOPlus :: (Floating u, Real u, Ord u, Mark t)
         => t -> Point2 u -> Picture u
dotOPlus attr pt = multi [dotCircle attr pt, dotPlus attr pt]


dotOCross :: (Floating u, Real u, Ord u, Mark t)
          => t -> Point2 u -> Picture u
dotOCross attr pt = multi [dotCircle attr pt, dotCross attr pt]

-}