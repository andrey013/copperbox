{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Dots
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Dots
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Dots
  ( 

  -- * Dots
    dotX
  , dotPlus
  , dotDiamond
  , dotDisk

  ) where



import Wumpus.Core
import Wumpus.Geometry

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



dotX :: (Ord u, Floating u, Real u, Stroke t) 
     => t -> Point2 u -> Picture u
dotX t pt = frameMulti $ map mkStroke [ls1, ls2]
  where
    mkStroke = ostroke t . lineSegmentToPath 
    ls1      = rotateAbout (pi/6) pt $ vlineSegmentBisect 2 pt
    ls2      = reflectYPlane pt ls1  -- wrong


dotPlus :: (Ord u, Floating u, Real u, Stroke t) 
        => t -> Point2 u -> Picture u
dotPlus t pt = frameMulti $ map mkStroke [ls1, ls2]
  where
    mkStroke = ostroke t . lineSegmentToPath 
    ls1   = vlineSegmentBisect 2 pt
    ls2   = hlineSegmentBisect 2 pt


dotDiamond :: (Floating u, Real u, Fill t) => t -> Point2 u -> Picture u
dotDiamond t pt = frame $ fillPolygon t $ regularPolygon 4 2 pt

dotDisk :: (Ord u, Fractional u, Ellipse t) => t -> Point2 u -> Picture u
dotDisk t pt = frame $ ellipse t pt 2 2