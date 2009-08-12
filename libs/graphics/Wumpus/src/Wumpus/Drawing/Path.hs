{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Path
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


module Wumpus.Drawing.Path where

import Wumpus.Core.Curve
import Wumpus.Core.Geometric
import Wumpus.Core.Instances ()
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Radian
import Wumpus.Core.Vector


import Data.AffineSpace
import Data.VectorSpace

import qualified Data.Foldable as F
import Data.List ( foldl' )
import Data.Sequence
import qualified Data.Sequence as S


-- A closed path will allow reopening by adding a LineTo the 
-- start point. While this is somewhat quirky, it is saves going
-- though (type state) hoops to make sure we can only do certain
-- operations on open or closed paths.

-- A ClosedPath holds its penultimate point, the last point being
-- the start point again. This is so we can open it again without
-- having to traverse the segments.


data PathEnd pt = EndPoint pt | PathClosed pt
  deriving (Eq,Show)

data Path pt = Path { 
       pathStart  :: pt,
       pathEnd    :: PathEnd pt,
       pathBody   :: Seq (PathSegment (Diff pt))
    }                    

instance (Show pt) => Show (Path pt) where
  showsPrec i s = showString "Path"


type DPath = Path Double


-- | Segments are stored as vectors. This makes moving a path a 
-- cheap operation - only the start point needs to be moved.
-- Of course extracting points is more expensive, but as our 
-- purpose is creating drawings (from paths) then it seems wise 
-- to favour /creation/ rather than /analysis/.
data PathSegment vec = RMoveTo  vec
                     | RLineTo  vec
                     | RCurveTo vec vec vec
  deriving (Eq,Show)



--------------------------------------------------------------------------------
-- Instances

-- Note the Pointwise instance is rather convoluted because Paths
-- contain two @Pt@ types Point2 and Vec2.

vecPointwise :: (ZeroPt pt, AffineSpace pt, Diff pt ~ v) => (v -> v) -> pt -> pt
vecPointwise f p = let dv = p .-. zeroPt in zeroPt .+^ f dv


instance (ZeroPt pt, AffineSpace pt) => Pointwise (Path pt) where
  type Pt (Path pt) = Diff pt
  pointwise f (Path s e sp) = 
    Path (vecPointwise f s) (pointwise f e) (pointwise f sp)



instance (ZeroPt pt, AffineSpace pt) => Pointwise (PathEnd pt) where
  type Pt (PathEnd pt) = Diff pt
  pointwise f (EndPoint p)   = EndPoint $ vecPointwise f p
  pointwise f (PathClosed p) = PathClosed $ vecPointwise f p


instance Pointwise a =>Pointwise (Seq a) where
  type Pt (Seq a) = Pt a
  pointwise f sp = fmap (pointwise f) sp

instance Pointwise (PathSegment v) where 
  type Pt (PathSegment v) = v
  pointwise f (RMoveTo v1)        = RMoveTo (f v1)
  pointwise f (RLineTo v1)        = RLineTo (f v1)
  pointwise f (RCurveTo v1 v2 v3) = RCurveTo (f v1) (f v2) (f v3)



-- Remember - for a closed path the (PathClosed a) holds the
-- penultimate point not the end point which is the start point 
-- again.

instance AffineSpace pt => HasPoints (Path pt) where
  type Pnt (Path pt) = pt
  extractPoints = pathExtractPoints
  startPoint (Path s _ _) = s
  endPoint (Path _ (EndPoint e)   _) = e
  endPoint (Path s (PathClosed _) _) = s  


-- Start with the start-point then extract the points from 
-- the path segments. The end-point in the top level type
-- is a duplicate of the last point in the path list, so 
-- ignore it.

pathExtractPoints :: AffineSpace pt 
                  => Path pt -> [pt]
pathExtractPoints (Path s e sp) = (s:) . snd $ F.foldr fn (initial e,[]) sp
  where
    initial (EndPoint p)   = p
    initial (PathClosed _) = s

    -- Note going backwards in the foldr, so 
    fn (RMoveTo v1)        (p,acc) = let p' = p .-^ v1 in (p',p':acc)
    fn (RLineTo v1)        (p,acc) = let p' = p .-^ v1 in (p',p':acc)
    fn (RCurveTo v1 v2 v3) (p,acc) = let p3 = p  .-^ v3 
                                         p2 = p3 .-^ v2
                                         p1 = p2 .-^ v1
                                     in (p1,p1:p2:p3:acc)


--------------------------------------------------------------------------------
-- Operations

newPath :: pt -> Path pt
newPath p = Path p (EndPoint p) empty



straightLine :: LineTo pt pt => LineSegment pt -> Path pt
straightLine (LS p1 p2) = newPath p1 `lineTo` p2


bezierPath :: CurveTo pt pt => Curve pt -> Path pt
bezierPath (Curve p0 p1 p2 p3) = newPath p0 `curveTo` (p1,p2,p3)  


tracePoints :: LineTo pt pt => [pt] -> Path pt
tracePoints (x:xs) = foldl' lineTo (newPath x) xs
tracePoints []     = error "tracePoints - cannot make a Path from an empty list"


reopenPath :: AffineSpace pt => Path pt -> Path pt
reopenPath (Path s (PathClosed p) sp) = Path s (EndPoint s) (sp |> RLineTo (s .-. p))
reopenPath path                       = path



closePath :: Path pt -> Path pt
closePath (Path s (EndPoint e) sp) = Path s (PathClosed e) sp
closePath path                     = path




displacePath :: (Num a, AffineSpace pt,
                Diff pt ~ v, Scalar v ~ a)
             => v -> Path pt -> Path pt
displacePath v (Path s end sp) = Path (s .+^ v) (disp end) sp
  where
   disp (EndPoint e)   = EndPoint $ e .+^ v
   disp (PathClosed e) = PathClosed $ e .+^ v



endGradient :: (Real a, Floating a, AffineSpace pt, LineAngle pt,
                Diff pt ~ v, Scalar v ~ a) 
            => Path pt -> Radian
endGradient path = let (p,e) = lastTwoPoints path in langle (LS p e)


startGradient :: (Real a, Floating a, AffineSpace pt, LineAngle pt,
                  Diff pt ~ v, Scalar v ~ a) 
              => Path pt -> Radian
startGradient path = let (s,p) = firstTwoPoints path in langle (LS s p)


lastTwoPoints :: (Num a, AffineSpace pt,
                  Diff pt ~ v, Scalar v ~ a) 
              => Path pt -> (pt,pt)
lastTwoPoints (Path s (PathClosed e) _) = (e,s)
lastTwoPoints (Path _ (EndPoint e) sp)  = fn $ viewr sp where
  fn (_ :> RMoveTo v)         = (e .-^ v, e) 
  fn (_ :> RLineTo v)         = (e .-^ v, e)
  fn (_ :> RCurveTo _ _ v3)   = (e .-^ v3,e)
  fn EmptyR                   = error "Path.lastTwoPoints - bad path, no segments"


firstTwoPoints :: (Num a, AffineSpace pt,
                   Diff pt ~ v, Scalar v ~ a) 
               => Path pt -> (pt,pt)
firstTwoPoints (Path s _ sp)  = fn $ viewl sp where
  fn (RMoveTo v :< _)         = (s,s .+^ v) 
  fn (RLineTo v :< _)         = (s,s .+^ v)
  fn (RCurveTo v1 _ _ :< _)   = (s,s .+^ v1)
  fn EmptyL                   = error "Path.firstTwoPoints - bad path, no segments"



-- | Make an open path from line segments. If consecutive segments
-- do not share (end-point, start-point) then insert a RMoveTo.
segmentPath :: (Eq pt, LineTo pt pt, AffineSpace pt) 
            => [LineSegment pt] -> Path pt
segmentPath []              = error "Path.segmentPath - empty list"
segmentPath ((LS p1 p2):xs) = foldl' fn (straightLine $ LS p1 p2) xs
  where
    fn path (LS p p') | p == endPoint path = path `lineTo` p'
                      | otherwise          = path `moveTo` p `lineTo` p'


class LineTo pt a where
  lineTo :: Path pt -> a -> Path pt
  moveTo :: Path pt -> a -> Path pt




instance Num a => LineTo (Point2 a) (Point2 a) where
  lineTo      (Path s (EndPoint e) sp)  p = Path s (EndPoint p) (sp |> RLineTo v)
    where v = p .-. e 
  lineTo path@(Path _ (PathClosed _) _) p = lineTo (reopenPath path) p

  moveTo      (Path s (EndPoint e) sp)  p = Path s (EndPoint p) (sp |> RMoveTo v)
    where v = p .-. e 
  moveTo path@(Path _ (PathClosed _) _) p = moveTo (reopenPath path) p
  

instance Num a => LineTo (Point2 a) (Vec2 a) where
  lineTo      (Path s (EndPoint e) sp)  v = Path s (EndPoint p) (sp |> RLineTo v)
    where p = e .+^ v
  lineTo path@(Path _ (PathClosed _) _) v = lineTo (reopenPath path) v

  moveTo      (Path s (EndPoint e) sp)  v = Path s (EndPoint p) (sp |> RMoveTo v)
    where p = e .+^ v
  moveTo path@(Path _ (PathClosed _) _) v = moveTo (reopenPath path) v


-- Package the points as a tuple to favour infix application 
-- rather than partial application.
class CurveTo pt a where
  curveTo :: Path pt -> (a,a,a) -> Path pt


instance Num a => CurveTo (Point2 a) (Point2 a) where
  curveTo      (Path s (EndPoint e) sp)  (p1,p2,p3) = 
      Path s (EndPoint p3) (sp |> RCurveTo v1 v2 v3)
    where 
      v1 = p1 .-. e
      v2 = p2 .-. p1
      v3 = p3 .-. p2
     
  curveTo path@(Path _ (PathClosed _) _) tup = curveTo (reopenPath path) tup




unPath :: Diff p ~ v => Path p -> [PathSegment v] 
unPath (Path _ _ sp) = F.toList sp