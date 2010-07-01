{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.CoreAdditions
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Additions due to be added to Wumpus.Core
-- (or maybe not...)
--
--------------------------------------------------------------------------------


module Wumpus.Extra.CoreAdditions
  (

    zeroPicture
  , bbcenter

  ) where


import Wumpus.Core


-- | Not sure about this - it is useful, but it might 
-- have bad 'semantics'...
--
-- Final decision - NO - this doesn't want to go into 
-- wumpus-core and should be removed from wumpus-extra!
-- 
zeroPicture :: Num u => Picture u
zeroPicture = blankPicture (BBox zeroPt zeroPt)

-- | Extract a point from the bounding box at the supplied 
-- cardinal position.
--
-- Maybe - waiting on Core.BoundingBox tidy up.
--
bbcenter :: Fractional a => BoundingBox a -> Point2 a
bbcenter (BBox (P2 x0 y0) (P2 x1 y1)) = P2 xMid  yMid
  where
    xMid      = x0 + 0.5 * (x1 - x0)
    yMid      = y0 + 0.5 * (y1 - y0)


{-
-- | Make a circle from Bezier curves - @n@ is the number of 
-- subdivsions per quadrant.
--
-- In CORE for 0.19.0
--
bezierCircle :: (Fractional u, Floating u) 
             => Int -> u -> Point2 u -> [Point2 u]
bezierCircle n radius pt = start $ subdivisions (n*4) (2*pi)
  where
    start (a:b:xs) = s : cp1 : cp2 : e : rest (b:xs)
      where (s,cp1,cp2,e) = bezierArc radius a b pt
                     
    start _        = [] 

    rest (a:b:xs)  = cp1 : cp2 : e : rest (b:xs)
      where (_,cp1,cp2,e) = bezierArc radius a b pt 

    rest _         = [] 

    subdivisions i a = 0 : take i (iterate (+one) one) 
      where  one  = a / fromIntegral i


-- In CORE for 0.19.0
--
wumpus_default_font :: FontAttr
wumpus_default_font = FontAttr "Courier" "Courier New" SVG_REGULAR 24


-}


