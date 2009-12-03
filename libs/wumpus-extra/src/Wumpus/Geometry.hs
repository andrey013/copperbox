{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Common interface to Wumpus.Core...
--
--------------------------------------------------------------------------------


module Wumpus.Geometry
  ( 
    module Wumpus.Geometry.Base
  , module Wumpus.Geometry.Curve
  , module Wumpus.Geometry.CoordinateGen
  , module Wumpus.Geometry.LineEquation
  , module Wumpus.Geometry.LineSegment
  , module Wumpus.Geometry.Polygon
  , lineSegmentToLine
  ) where

import Wumpus.Geometry.Base
import Wumpus.Geometry.Curve
import Wumpus.Geometry.CoordinateGen
import Wumpus.Geometry.LineEquation
import Wumpus.Geometry.LineSegment
import Wumpus.Geometry.Polygon


-- Avoid circular deps...

lineSegmentToLine :: Num u => LineSegment u -> Line u
lineSegmentToLine (LS2 p0 p1) = line p0 p1


