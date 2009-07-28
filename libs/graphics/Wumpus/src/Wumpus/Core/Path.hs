{-# LANGUAGE TypeFamilies               #-}
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
-- Paths 
--
--------------------------------------------------------------------------------


module Wumpus.Core.Path where


import Wumpus.Core.Curve
import Wumpus.Core.Geometric
import Wumpus.Core.Line
import Wumpus.Core.Point

import qualified Data.Foldable as F
import Data.Sequence


-- | Unlike PGF/TikZ for instance (and PostScript itself), we don\'t 
-- have MoveTo gaps in the path. 

data Path a = Path { 
       pathStart  :: Point2 a,
       pathEnd    :: Point2 a,
       pathBody   :: Seq (PathSegment a)
    }                    

type DPath = Path Double

data PathSegment a = PLine (LineSegment Point2 a)
                   | PCurve (Curve a)
  deriving (Eq,Show)

newPath :: Point2 a -> Path a
newPath p = Path p p empty

curveTo :: Path a -> (Point2 a -> Curve a) -> Path a
curveTo (Path s e ps) cf = Path s (endPoint c) (ps |> PCurve c)
  where c = cf e

lineTo :: Path a -> (Point2 a -> LineSegment Point2 a) -> Path a
lineTo (Path s e ps) lf = Path s (endPoint l) (ps |> PLine l)
  where l = lf e

-- temporary...
unPath :: Path a -> [Either (LineSegment Point2 a) (Curve a)]
unPath (Path _ _ ps) = F.foldr fn [] ps
  where
    fn (PLine l)  xs = Left l : xs
    fn (PCurve c) xs = Right c : xs 