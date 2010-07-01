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


  ) where


import Wumpus.Core

import Wumpus.Extra.Base
import Wumpus.Extra.BasicObjects
import Wumpus.Extra.Marks

import Data.AffineSpace


-- NOTES
--
-- Affine transforming Points, LineSegments etc. before
-- they become pictures is _GOOD_! The calculations are done in 
-- Wumpus and so don't cause extra (gsave... grestore) in 
-- PostScript.
--



dotHLine :: (Fractional u, Floating u, Ord u, Mark t)
         => t -> Point2 u -> Picture u 
dotHLine attr pt = 
  frame $ mostroke attr $ lineSegmentToPath $ hlineSegmentMid (lwX10 attr) pt


dotVLine :: (Fractional u, Floating u, Ord u, Mark t)
         => t -> Point2 u -> Picture u 
dotVLine attr pt = 
  frame $ mostroke attr $ lineSegmentToPath $ vlineSegmentMid (lwX10 attr) pt


dotX :: (Ord u, Floating u, Real u, Fractional u, Mark t) 
     => t -> Point2 u -> Picture u
dotX attr pt = frameMulti $ map mkStroke [ls1, ls2]
  where
    mkStroke = mostroke attr . lineSegmentToPath 
    ls1      = rotateAbout (pi/6) pt $ vlineSegmentMid (lwX10 attr) pt
    ls2      = reflectYPlane pt ls1  -- wrong


dotPlus :: (Ord u, Floating u, Real u, Fractional u, Mark t) 
        => t -> Point2 u -> Picture u
dotPlus attr pt = frameMulti $ map mkStroke [ls1, ls2]
  where
    mkStroke = mostroke attr . lineSegmentToPath 
    ls1   = vlineSegmentMid (lwX10 attr) pt
    ls2   = hlineSegmentMid (lwX10 attr) pt

dotCross :: (Ord u, Floating u, Real u, Fractional u, Mark t)
         => t -> Point2 u -> Picture u
dotCross attr pt = rotate45About pt $ dotPlus attr pt


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

