{-# LANGUAGE TypeFamilies               #-}
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

import Wumpus.Core.Colour
import Wumpus.Core.Curve
import Wumpus.Core.Geometric
import Wumpus.Core.Instances ()
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Radian
import Wumpus.Core.Vector

import Wumpus.Drawing.GraphicsState

import Data.AffineSpace

import qualified Data.Foldable as F
import Data.List ( foldl' )
import Data.Sequence
import qualified Data.Sequence as S


-- /Visual path/
data VPath a = VPath { 
        pathAttr :: PathAttr, 
        pathSegments :: (Path a)
      }
  deriving (Eq,Show)

data PathAttr = Stroke DRGB Pen
              | Fill   DRGB 
              | Clip
  deriving (Eq,Show) 

stroke :: PathAttr
stroke = Stroke wumpusBlack newPen

fill :: PathAttr
fill = Fill wumpusBlack

mapPath :: (Path a -> Path b) -> VPath a -> VPath b
mapPath f (VPath attr sp) = VPath attr (f sp) 

-- A closed path will allow reopening by adding a LineTo the 
-- start point. While this is somewhat quirky, it is saves going
-- though (type state) hoops to make sure we can only do certain
-- operations on open or closed paths.

-- A ClosedPath holds its penultimate point, the last point being
-- the start point again. This is so we can open it again without
-- having to traverse the segments.


data PathEnd a = EndPoint (Point2 a) | PathClosed (Point2 a)
  deriving (Eq,Show)

data Path a = Path { 
       pathStart  :: Point2 a,
       pathEnd    :: PathEnd a,
       pathBody   :: Seq (PathSegment a)
    }                    
  deriving (Eq,Show)


type DVPath = VPath Double


-- | Segments are stored as vectors. This makes moving a path a 
-- cheap operation - only the start point needs to be moved.
-- Of course extracting points is more expensive, but as our 
-- purpose is creating drawings (from paths) then it seems wise 
-- to favour /creation/ rather than /analysis/.
data PathSegment a = RMoveTo  (Vec2 a) 
                   | RLineTo  (Vec2 a)
                   | RCurveTo (Vec2 a) (Vec2 a) (Vec2 a)
  deriving (Eq,Show)



--------------------------------------------------------------------------------
-- Instances

-- Note the Pointwise instance is rather convoluted because Paths
-- contain two @Pt@ types Point2 and Vec2.

vecPointwise :: Num a => (Vec2 a -> Vec2 a) -> Point2 a -> Point2 a
vecPointwise f p = let dv = p .-. zeroPt in zeroPt .+^ f dv


instance Num a => Pointwise (Path a) where
  type Pt (Path a) = Vec2 a
  pointwise f (Path s e sp) = 
    Path (vecPointwise f s) (pointwise f e) (pointwise f sp)



instance Num a => Pointwise (PathEnd a) where
  type Pt (PathEnd a) = Vec2 a
  pointwise f (EndPoint p)   = EndPoint $ vecPointwise f p
  pointwise f (PathClosed p) = PathClosed $ vecPointwise f p


instance Pointwise a =>Pointwise (Seq a) where
  type Pt (Seq a) = Pt a
  pointwise f sp = fmap (pointwise f) sp

instance Pointwise (PathSegment a) where 
  type Pt (PathSegment a) = Vec2 a
  pointwise f (RMoveTo v1)        = RMoveTo (f v1)
  pointwise f (RLineTo v1)        = RLineTo (f v1)
  pointwise f (RCurveTo v1 v2 v3) = RCurveTo (f v1) (f v2) (f v3)



-- Remember - for a closed path the (PathClosed a) holds the
-- penultimate point not the end point which is the start point 
-- again.

instance Num a => HasPoints (Path a) where
  type Pnt (Path a) = Point2 a
  extractPoints = pathExtractPoints
  startPoint (Path s _ _) = s
  endPoint (Path _ (EndPoint e)   _) = e
  endPoint (Path s (PathClosed _) _) = s  


-- Start with the start-point then extract the points from 
-- the path segments. The end-point in the top level type
-- is a duplicate of the last point in the path list, so 
-- ignore it.

pathExtractPoints :: Num a => Path a -> [Point2 a]
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

newPath :: Point2 a -> Path a
newPath p = Path p (EndPoint p) empty

straightLine :: Num a => LineSegment Point2 a -> Path a
straightLine (LS p1 p2) = newPath p1 `lineTo` p2


bezierPath :: Num a => Curve a -> Path a
bezierPath (Curve p0 p1 p2 p3) = newPath p0 `curveTo` (p1,p2,p3)  


tracePoints :: Num a => [Point2 a] -> Path a
tracePoints (x:xs) = foldl' lineTo (newPath x) xs
tracePoints []     = error "tracePoints - cannot make a Path from an empty list"


reopenPath :: Num a => Path a -> Path a
reopenPath (Path s (PathClosed p) sp) = Path s (EndPoint s) (sp |> RLineTo (s .-. p))
reopenPath path                       = path

closePath :: Path a -> Path a
closePath (Path s (EndPoint e) sp) = Path s (PathClosed e) sp
closePath path                     = path


displacePath :: Num a => Vec2 a -> Path a -> Path a
displacePath v (Path s end sp) = Path (s .+^ v) (disp end) sp
  where
   disp (EndPoint e)   = EndPoint $ e .+^ v
   disp (PathClosed e) = PathClosed $ e .+^ v


endGradient :: (Real a, Floating a) => Path a -> Radian
endGradient path = let (p,e) = lastTwoPoints path in langle (LS p e)

startGradient :: (Real a, Floating a) => Path a -> Radian
startGradient path = let (s,p) = firstTwoPoints path in langle (LS s p)

lastTwoPoints :: Num a => Path a -> (Point2 a, Point2 a)
lastTwoPoints (Path s (PathClosed e) _) = (e,s)
lastTwoPoints (Path _ (EndPoint e) sp)  = fn $ viewr sp where
  fn (_ :> RMoveTo v)         = (e .-^ v, e) 
  fn (_ :> RLineTo v)         = (e .-^ v, e)
  fn (_ :> RCurveTo _ _ v3)   = (e .-^ v3,e)
  fn EmptyR                   = error "Path.lastTwoPoints - bad path, no segments"

firstTwoPoints :: Num a => Path a -> (Point2 a,Point2 a)
firstTwoPoints (Path s _ sp)  = fn $ viewl sp where
  fn (RMoveTo v :< _)         = (s,s .+^ v) 
  fn (RLineTo v :< _)         = (s,s .+^ v)
  fn (RCurveTo v1 _ _ :< _)   = (s,s .+^ v1)
  fn EmptyL                   = error "Path.firstTwoPoints - bad path, no segments"

-- | Make an open path from line segments. If consecutive segments
-- do not share (end-point, start-point) then insert a RMoveTo.
segmentPath :: Num a => [LineSegment Point2 a] -> Path a
segmentPath []              = error "Path.segmentPath - empty list"
segmentPath ((LS p1 p2):xs) = foldl' fn (straightLine $ LS p1 p2) xs
  where
    fn path (LS p p') | p == endPoint path = path `lineTo` p'
                      | otherwise          = path `moveTo` p `lineTo` p'

class LineTo t where
  lineTo :: Num a => Path a -> t a -> Path a
  moveTo :: Num a => Path a -> t a -> Path a


-- Package the points as a tuple to favour infix application 
-- rather than partial application.
class CurveTo t where
  curveTo :: Num a => Path a -> (t a, t a, t a) -> Path a



instance LineTo Point2 where
  lineTo      (Path s (EndPoint e) sp)  p = Path s (EndPoint p) (sp |> RLineTo v)
    where v = p .-. e 
  lineTo path@(Path _ (PathClosed _) _) p = lineTo (reopenPath path) p

  moveTo      (Path s (EndPoint e) sp)  p = Path s (EndPoint p) (sp |> RMoveTo v)
    where v = p .-. e 
  moveTo path@(Path _ (PathClosed _) _) p = moveTo (reopenPath path) p

instance CurveTo Point2 where
  curveTo      (Path s (EndPoint e) sp)  (p1,p2,p3) = 
      Path s (EndPoint p3) (sp |> RCurveTo v1 v2 v3)
    where 
      v1 = p1 .-. e
      v2 = p2 .-. p1
      v3 = p3 .-. p2
     
  curveTo path@(Path _ (PathClosed _) _) tup = curveTo (reopenPath path) tup
  

instance LineTo Vec2 where
  lineTo      (Path s (EndPoint e) sp)  v = Path s (EndPoint p) (sp |> RLineTo v)
    where p = e .+^ v
  lineTo path@(Path _ (PathClosed _) _) v = lineTo (reopenPath path) v

  moveTo      (Path s (EndPoint e) sp)  v = Path s (EndPoint p) (sp |> RMoveTo v)
    where p = e .+^ v
  moveTo path@(Path _ (PathClosed _) _) v = moveTo (reopenPath path) v


changeColour :: DRGB -> PathAttr -> PathAttr  
changeColour c (Stroke _ pen)   = Stroke c pen
changeColour c (Fill   _)       = Fill c 
changeColour _ Clip             = Clip



{-
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


-}

unPath :: Path a -> [PathSegment a] 
unPath (Path _ _ sp) = F.toList sp