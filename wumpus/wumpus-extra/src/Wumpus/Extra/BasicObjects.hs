{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.BasicObjects
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Basic graphics objects (lines segments, curves, polygons...)
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.BasicObjects
  ( 

  -- * Data types
    LineSegment(..)
  , DLineSegment

  , CubicBezier(..)
  , DCubicBezier

  , Polygon
  , DPolygon


  -- * Line construction
  , lineSegment
  , lineSegmentV
  , hlineSegment
  , vlineSegment
  , alineSegment
  , hlineSegmentMid
  , vlineSegmentMid

  , expandLineSegment
  , lineSegmentToPath


  -- * Bezier construction
  , cubicBezier

  -- * Operations

  , curveToPath
  , curvesToPath

  , circlePath 

  , bend
  , tighten
  , tildeCurve
  

  -- * Polygon construction
  , square
  , rectangle
  , regularPolygon
  , isoscelesTriangle

  -- * Drawing polygons
  , fillPolygon
  , strokePolygon

  , strokeRoundedPolygon
  , fillRoundedPolygon

  ) where


import Wumpus.Core
import Wumpus.Extra.Base
import Wumpus.Extra.Utils


import Data.AffineSpace
import Data.VectorSpace

-- | A straight line between 2 points.
--
data LineSegment u = LS2 (Point2 u) (Point2 u)
  deriving (Eq,Show)

type DLineSegment = LineSegment Double


-- | A cubic Bezier curve.
--
data CubicBezier u = CubicBezier (Point2 u) (Point2 u) (Point2 u) (Point2 u)
  deriving (Eq,Show)

type DCubicBezier = CubicBezier (Point2 Double)


-- | A polygon - represented internally by a list of points.
--
newtype Polygon u = Polygon { vertexList :: [Point2 u] }
  deriving (Eq,Show)

type DPolygon = Polygon Double


--------------------------------------------------------------------------------
-- Instances

type instance DUnit (LineSegment u) = u
type instance DUnit (CubicBezier u) = u
type instance DUnit (Polygon u)     = u


instance Functor LineSegment where
  fmap f (LS2 p0 p1) = LS2 (fmap f p0) (fmap f p1)

instance Functor CubicBezier where
  fmap f (CubicBezier p0 p1 p2 p3) = 
      CubicBezier (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)

--------------------------------------------------------------------------------
-- Pointwise instances

instance Pointwise (LineSegment u) where
  type Pt (LineSegment u) = Point2 u
  pointwise f (LS2 p0 p1) = LS2 (f p0) (f p1)

instance Pointwise (CubicBezier u) where
  type Pt (CubicBezier u) = Point2 u
  pointwise f (CubicBezier p0 p1 p2 p3) = CubicBezier (f p0) (f p1) (f p2) (f p3)

instance Pointwise (Polygon a) where
  type Pt (Polygon a) = Point2 a
  pointwise f (Polygon xs) = Polygon $ map f xs

--------------------------------------------------------------------------------
-- Affine instances

instance Num u => MatrixMult (LineSegment u) where
  (*#) m3'3 (LS2 p0 p1) = LS2 (m3'3 *# p0) (m3'3 *# p1)

instance (Floating u, Real u) => Rotate (LineSegment u) where
  rotate ang = pointwise (rotate ang) 

instance (Floating u, Real u) => RotateAbout (LineSegment u) where
  rotateAbout r pt = pointwise (rotateAbout r pt) 

instance (Floating u, Real u) => Scale (LineSegment u) where
  scale x y = pointwise (scale x y) 

instance (Floating u, Real u) => Translate (LineSegment u) where
  translate x y = pointwise (translate x y) 



instance Num u => MatrixMult (CubicBezier u) where
  (*#) m3'3 (CubicBezier p0 p1 p2 p3) = 
      CubicBezier (m3'3 *# p0) (m3'3 *# p1) (m3'3 *# p2) (m3'3 *# p3)

instance (Floating u, Real u) => Rotate (CubicBezier u) where
  rotate ang = pointwise (rotate ang) 

instance (Floating u, Real u) => RotateAbout (CubicBezier u) where
  rotateAbout r pt = pointwise (rotateAbout r pt) 

instance (Floating u, Real u) => Scale (CubicBezier u) where
  scale x y = pointwise (scale x y) 

instance (Floating u, Real u) => Translate (CubicBezier u) where
  translate x y = pointwise (translate x y) 


instance Num u => MatrixMult (Polygon u) where
  (*#) m3'3 (Polygon ps) = Polygon $ map (\pt -> m3'3 *# pt) ps

instance (Floating u, Real u) => Rotate (Polygon u) where
  rotate ang = pointwise (rotate ang) 

instance (Floating u, Real u) => RotateAbout (Polygon u) where
  rotateAbout r pt = pointwise (rotateAbout r pt) 

instance (Floating u, Real u) => Scale (Polygon u) where
  scale x y = pointwise (scale x y) 

instance (Floating u, Real u) => Translate (Polygon u) where
  translate x y = pointwise (translate x y) 


--------------------------------------------------------------------------------
-- Geometry instances

-- Reverse the direction of a line segment.

instance Converse (LineSegment u) where
  converse (LS2 p0 p1) = LS2 p1 p0

instance (Floating u, Real u) => CCWAngle (LineSegment u) where
  ccwAngle (LS2 (P2 x y) (P2 x' y')) = toRadian $ atan $ (y'-y) / (x'-x) 

instance (Floating u, InnerSpace v, v ~ Vec2 u) => 
    ObjectLength (LineSegment u) where
  objectLength (LS2 p0 p1) = distance p1 p0

instance Fractional u => Midpoint (LineSegment u) where
  midpoint (LS2 p0 p1) = midpointBetween p0 p1


instance Converse (CubicBezier u) where
  converse (CubicBezier p0 p1 p2 p3) = CubicBezier p3 p2 p1 p0


instance Converse (Polygon u) where
  converse (Polygon ps) = Polygon $ reverse ps

instance ExtractPath (Polygon u) where
  extractPath = vertexPath . vertexList

instance (Num u, Ord u) => Boundary (Polygon u) where
  boundary = trace . vertexList


--------------------------------------------------------------------------------
-- Line construction


lineSegment :: Point2 u -> Point2 u -> LineSegment u
lineSegment = LS2


-- | Create a line with start point @p@ and end point @p .+^ v@.
lineSegmentV :: Num u => Vec2 u -> Point2 u -> LineSegment u
lineSegmentV v p = lineSegment p (p .+^ v)


-- | Horizontal line of length @a@ from point @p@.
hlineSegment :: Num u => u -> Point2 u -> LineSegment u
hlineSegment a = lineSegmentV (hvec a)


-- | Vertical line segment of length @a@ from point @p@.
vlineSegment :: Num u => u -> Point2 u -> LineSegment u
vlineSegment a = lineSegmentV (vvec a)


-- | A line segment in the direction angle @theta@ from x-axis, of 
-- length @a@, starting at point @p@.
alineSegment :: Floating u => Radian -> u -> Point2 u -> LineSegment u
alineSegment theta a = lineSegmentV (avec theta a)



-- | Horizontal line of length @a@ centered at point @p@.
hlineSegmentMid :: Fractional u => u -> Point2 u -> LineSegment u
hlineSegmentMid n (P2 x y) = hlineSegment n  (P2 (x-0.5*n) y)


-- | Vertical line of length @a@ centered at point @p@.
vlineSegmentMid :: Fractional u => u -> Point2 u -> LineSegment u
vlineSegmentMid n (P2 x y) = vlineSegment n (P2 x (y-0.5*n))



--------------------------------------------------------------------------------
-- Line operations



-- | Expand line segment.
expandLineSegment :: (Floating u, Real u, InnerSpace v, v ~ Vec2 u) 
                  => u -> LineSegment u -> LineSegment u
expandLineSegment n line =  LS2 (p .-^ v) (p .+^ v) where
    l = objectLength line
    v = avec (ccwAngle line) (l * 0.5 * n)
    p = midpoint line



--------------------------------------------------------------------------------
-- To picture types

lineSegmentToPath :: LineSegment u -> Path u
lineSegmentToPath (LS2 p1 p2) = vertexPath [p1,p2]

-- A segments-to-path function will have undefined behaviour 
-- when end and start points of lines don't agree. 
--
-- > segmentsToPath :: [LineSegment u] -> Path u
--
-- So there isn't one...
--


--------------------------------------------------------------------------------
-- construction

-- TODO - any useful 'smart' constructor for Bezier curves?

cubicBezier :: Point2 u -> Point2 u -> Point2 u -> Point2 u -> CubicBezier u
cubicBezier = CubicBezier 



--------------------------------------------------------------------------------
-- operations

curveToPath :: CubicBezier u -> Path u
curveToPath (CubicBezier p0 p1 p2 p3) = path p0 [curveTo p1 p2 p3]



curvesToPath :: [CubicBezier u] -> Path u
curvesToPath []                     = error $ "curvesToPath - empty list"
curvesToPath (CubicBezier p0 p1 p2 p3:cs) = 
   path p0 (curveTo p1 p2 p3 : map fn cs) where 
      fn (CubicBezier _ u v w) = curveTo u v w


adaptPoints :: [Point2 u] -> [CubicBezier u]
adaptPoints = start where
   start (p0:p1:p2:p3:xs) = CubicBezier p0 p1 p2 p3 : rest p3 xs
   start _                = error $ "adapt -- too short"
   
   rest p0 (p1:p2:p3:xs)  = CubicBezier p0 p1 p2 p3 : rest p3 xs
   rest _  _              = []



--------------------------------------------------------------------------------

circlePath :: (Fractional u, Floating u) 
           => Int -> u -> Point2 u -> Path u
circlePath n radius = curvesToPath . adaptPoints . bezierCircle n radius



-- @bend@ seems most intuitive for \'humps\' - maybe it should 
-- only take one angle...

bend :: (Floating u, Real u, InnerSpace (Vec2 u))
     => Radian -> Radian -> Point2 u -> Point2 u -> CubicBezier u
bend oang iang u v = cubicBezier u a b v
  where
    half_dist = 0.5 * distance u v 
    theta     = langle u v
    a         = u .+^ avec (theta + oang) half_dist 
    b         = v .+^ avec (theta + iang) half_dist


tighten :: Num u => Vec2 u -> Vec2 u -> CubicBezier u -> CubicBezier u
tighten u v (CubicBezier p0 p1 p2 p3) = CubicBezier p0 (p1 .+^ u) (p2 .+^ v) p3
  -- ang = langle p0 p4


-- | Create a tilde (sinusodial) curve about the horizontal plane.
-- 
-- This one is rather simplistic - single one phase curve with no
-- subdivision...
-- 
-- There are better ways to plot things
--
tildeCurve :: (Floating u, AffineSpace (pt u), Converse (Vec2 u)) 
           => u -> Point2 u -> CubicBezier u
tildeCurve w = \pt -> let endpt = pt .+^ hvec w
                      in cubicBezier pt (pt .+^ v) (endpt .+^ converse v) endpt
  where 
    v = avec (pi/4) (w/2)

--------------------------------------------------------------------------------


-- | Create a square of side length @n@ with bottom-left corner 
-- located at the supplied point.
--
square :: Num u => u -> Point2 u -> Polygon u
square w bl = Polygon $ sequence [id,v1,v2,v3] bl where
    v1 = (.+^ hvec w)
    v2 = (.+^ V2 w w)
    v3 = (.+^ vvec w)

-- | Create a rectangle of width @w@ and height @h@ with the 
-- bottom-left corner located at the supplied point.
--
rectangle :: Num u => u -> u -> Point2 u -> Polygon u
rectangle w h bl = Polygon $ sequence [id,v1,v2,v3] bl where
    v1 = (.+^ hvec w)
    v2 = (.+^ V2 w h)
    v3 = (.+^ vvec h)



-- | Create a regular polygon with @n@ sides and /radius/ @r@ 
-- centered at the supplied point.
regularPolygon :: (Floating u, Real u)
               => Int -> u -> Point2 u -> Polygon u
regularPolygon = Polygon `ooo` circularAbout




-- | @isocelesTriangle bw h pt@
--
isoscelesTriangle :: Fractional u => u -> u -> Point2 u -> Polygon u
isoscelesTriangle bw h pt = Polygon [br,top,bl] 
  where
    hh  = h/2
    hw  = bw/2
    top = pt .+^ vvec hh
    br  = pt .+^ V2   hw  (-hh)
    bl  = pt .+^ V2 (-hw) (-hh)


--------------------------------------------------------------------------------
-- Drawing polygons

fillPolygon :: (Fill t, Num u, Ord u) => t -> Polygon u -> Primitive u
fillPolygon props = fill props . (vertexPath . vertexList) 

strokePolygon :: (Stroke t, Num u, Ord u) => t -> Polygon u -> Primitive u
strokePolygon props = cstroke props . (vertexPath . vertexList) 

fillRoundedPolygon :: (Fill t, Num u, Ord u, Floating u, Real u,
                       InnerSpace (Vec2 u)) 
                   => u -> t -> Polygon u -> Primitive u
fillRoundedPolygon d props = fill props . roundedPolyPath d

strokeRoundedPolygon :: (Stroke t, Num u, Ord u, Floating u, Real u,
                         InnerSpace (Vec2 u)) 
                     => u -> t -> Polygon u -> Primitive u
strokeRoundedPolygon d props = cstroke props . roundedPolyPath d


roundedPolyPath :: (Floating u, Real u, InnerSpace (Vec2 u)) 
               => u -> Polygon u -> Path u
roundedPolyPath _ (Polygon ps) | length ps < 3 = error $ 
    "roundedPolyPath - vertex list too short - " ++ show (length ps)

roundedPolyPath d (Polygon ps) = mkPath1 corner_quads
  where
    corner_quads    = map (roundCorner d) $ partCycle3 ps

    mkPath1 []                 = error "roundedPolygon - unreachable"
    mkPath1 ((p0,p1,p2,p3):xs) = path p0 $ curveTo p1 p2 p3 : mkSegs xs

    mkSegs []                  = []
    mkSegs ((p0,p1,p2,p3):xs)  = lineTo p0 : curveTo p1 p2 p3 : mkSegs xs


roundCorner :: (Floating u, Real u, InnerSpace (Vec2 u)) 
            => u -> (Point2 u, Point2 u, Point2 u)
            -> (Point2 u, Point2 u, Point2 u, Point2 u)
roundCorner d (p0,p1,p2) = (p0_1, cp1,cp2,p2_1) 
  where
    p0_1   = p1 .+^ (avec (langle p1 p0) d)   -- pt between p0 and p1
    p2_1   = p1 .+^ (avec (langle p1 p2) d)   -- pt between p2 and p1

    theta = vangle (p0 .-. p0_1) (p2 .-. p2_1)

    d'    = rescale (0,pi) (0,d) (realToFrac theta)
    
    cp1   = p0_1 .+^ (avec (langle p0 p1) d')
    cp2   = p2_1 .+^ (avec (langle p2 p1) d')



partCycle3 :: [a] -> [(a,a,a)]
partCycle3 (a:b:c:cs) = step a b c cs
  where
    step t u v []     = [(t,u,v), (u,v,a), (v,a,b)]
    step t u v (x:xs) = (t,u,v) : step u v x xs
partCycle3 _xs        = error $ "partCycle3 - list too short"

--------------------------------------------------------------------------------