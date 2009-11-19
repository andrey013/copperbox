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
  , dotCross
  , dotDiamond
  , dotDisk
  , dotSquare

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

dotCross :: (Ord u, Floating u, Real u, Stroke t) 
         => t -> Point2 u -> Picture u
dotCross t pt = rotate45About pt $ dotPlus t pt


dotDiamond :: (Floating u, Real u, Fill t) => t -> Point2 u -> Picture u
dotDiamond t pt = frame $ fillPolygon t $ regularPolygon 4 2 pt

dotDisk :: (Ord u, Fractional u, Ellipse t) => t -> Point2 u -> Picture u
dotDisk t pt = frame $ ellipse t pt 2 2


dotSquare :: (Num u, Ord u, Fill t) => t -> Point2 u -> Picture u
dotSquare t p = frame $ fillPolygon t $ square 4 (p .-^ (V2 2 2))

--------------------------------------------------------------------------------
-- Alternative

-- Dots as functions extracting LineWidth from the /environment/.
-- 

type LineWidth u = u

type Dot u = LineWidth u -> Picture u


squareDot :: (Num u, Ord u, Stroke t) => t -> Point2 u -> Dot u
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
