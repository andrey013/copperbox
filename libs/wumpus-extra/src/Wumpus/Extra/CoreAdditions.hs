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

-- TextLabel has questionable arg order... {FIXED}

-- Not sure about this - it is useful, but it might 
-- have bad 'semantics' 
--
zeroPicture :: Num u => Picture u
zeroPicture = blankPicture (BBox zeroPt zeroPt)

-- | Extract a point from the bounding box at the supplied 
-- cardinal position.
bbcenter :: Fractional a => BoundingBox a -> Point2 a
bbcenter (BBox (P2 x0 y0) (P2 x1 y1)) = P2 xMid  yMid
  where
    xMid      = x0 + 0.5 * (x1 - x0)
    yMid      = y0 + 0.5 * (y1 - y0)