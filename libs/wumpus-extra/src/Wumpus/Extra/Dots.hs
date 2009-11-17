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
  ) where



import Wumpus.Core
import Wumpus.Extra.Drawing ( frameMulti, reflectYPlane ) -- TEMPORARY
import Wumpus.Geometry

-- NOTES
-- dotX -- colour, linewidth, ...
-- makes sense to supply the origin, then we aren't having to 
-- do extra affine transformations (gsave, concat, ..., grestore).
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
    ls1      = rotateAbout (pi/6) pt $ vlineSegmentC 2 pt
    ls2      = reflectYPlane pt ls1  -- wrong


dotPlus :: (Ord u, Floating u, Real u, Stroke t) 
        => t -> Point2 u -> Picture u
dotPlus t pt = frameMulti $ map mkStroke [ls1, ls2]
  where
    mkStroke = ostroke t . lineSegmentToPath 
    ls1   = vlineSegmentC 2 pt
    ls2   = hlineSegmentC 2 pt

