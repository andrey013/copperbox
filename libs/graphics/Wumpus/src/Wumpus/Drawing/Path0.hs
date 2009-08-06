{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Path0
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Paths - frozen for compatibility
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.Path0 where


import Wumpus.Core.Curve
import Wumpus.Core.Geometric
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise

import qualified Data.Foldable as F
import Data.Sequence
import qualified Data.Sequence as S

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



--------------------------------------------------------------------------------
-- Instances


instance Pointwise (Path a) where
  type Pt (Path a) = Point2 a
  pointwise f (Path s e sp) = Path (f s) (f e) (pointwise f sp)

instance Pointwise a =>Pointwise (Seq a) where
  type Pt (Seq a) = Pt a
  pointwise f sp = fmap (pointwise f) sp

instance Pointwise (PathSegment a) where 
  type Pt (PathSegment a) = Point2 a
  pointwise f (PLine l)  = PLine (pointwise f l)
  pointwise f (PCurve c) = PCurve (pointwise f c)




instance HasPoints (Path a) where
  type Pnt (Path a) = Point2 a
  extractPoints = pathExtractPoints
  endPoint (Path _ e _)            = e
  startPoint (Path s _ _)          = s


-- Start and end points between consecutive segments are /shared/,
-- so work backwards dropping the start point at each step
pathExtractPoints :: Path a -> [Point2 a]
pathExtractPoints (Path _ _ sp) 
    | S.null sp = []
    | otherwise = F.foldr fn [] sp 
  where 
    fn (PLine (LS p0 p1))           ls = p0:p1: safeTail ls
    fn (PCurve (Curve p0 p1 p2 p3)) ls = p0:p1:p2:p3: safeTail ls   
    safeTail []     = []
    safeTail (_:xs) = xs

--------------------------------------------------------------------------------
-- Operations

newPath :: Point2 a -> Path a
newPath p = Path p p empty

curveTo :: Path a -> (Point2 a -> Curve a) -> Path a
curveTo (Path s e sp) cf = Path s (endPoint c) (sp |> PCurve c)
  where c = cf e

lineTo :: Path a -> (Point2 a -> LineSegment Point2 a) -> Path a
lineTo (Path s e sp) lf = Path s (endPoint l) (sp |> PLine l)
  where l = lf e

straight :: Path a -> Point2 a -> Path a
straight (Path s e sp) p = Path s p (sp |> PLine (LS e p))

curved :: Path a -> (Point2 a, Point2 a, Point2 a) -> Path a
curved (Path s e sp) (p1,p2,p3) = Path s p3 (sp |> PCurve (Curve e p1 p2 p3))


-- temporary...
unPath :: Path a -> [Either (LineSegment Point2 a) (Curve a)]
unPath (Path _ _ sp) = F.foldr fn [] sp
  where
    fn (PLine l)  xs = Left l : xs
    fn (PCurve c) xs = Right c : xs 