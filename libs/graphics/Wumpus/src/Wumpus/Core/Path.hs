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


-- Hughes list -- 
type H a = [a] -> [a]


iterPath :: (a -> st) -> (b -> st) -> [Either (st -> a) (st -> b)] -> st -> [Either a b]
iterPath extl extr funs st0 = step funs st0 where
  step []           _ = []
  step (Left f:fs)  a = let ans = f a in Left ans : step fs (extl ans)
  step (Right f:fs) a = let ans = f a in Right ans : step fs (extr ans)


rep :: a -> H a
rep ln = (ln:)

abs :: H (CoPathSegment a) -> Point2 a -> [PathSegment a]
abs f start_pt = iterPath endPoint endPoint (f []) start_pt


type CoPathSegment a  = Either (CoLineSegment Point2 a) (CoCurve a)
type PathSegment   a  = Either (LineSegment Point2 a) (Curve a)

withLine :: CoLineSegment Point2 a -> H (CoPathSegment a)
withLine = rep . Left


withCurve :: CoCurve a -> H (CoPathSegment a)
withCurve = rep . Right

{-
data PathSegment a = PLine (LineSegment Point2 a)
                   | PCurve  (Curve a)
  deriving (Eq,Show)

-}

