{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Path
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Path - currently just a (not very pleasing) experiment to make paths 
-- with /function composition/.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Path where


import Wumpus.Core.Curve
import Wumpus.Core.Geometric
import Wumpus.Core.Line
import Wumpus.Core.Point

import Prelude hiding ( abs )

data Path = Path 

data PathSegment a = PLine (LineSegment Point2 a)
                   | PCurve  (Curve a)
  deriving (Eq,Show)



