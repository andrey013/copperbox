{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
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
  , bezierArc
  , bezierCircle

  , curveToPath
  , curvesToPath

  , circlePath 

  -- * Polygon construction
  , square
  , rectangle
  , regularPolygon
  , isoscelesTriangle

  -- * Drawing polygons
  , fillPolygon
  , strokePolygon

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





--------------------------------------------------------------------------------
-- construction

-- TODO - any useful 'smart' constructor for Bezier curves?

cubicBezier :: Point2 u -> Point2 u -> Point2 u -> Point2 u -> CubicBezier u
cubicBezier = CubicBezier 


-- | Create an arc - this construction is the analogue of 
-- PostScript\'s @arc@ command, but the arc is created as a 
-- Bezier curve so it should span less than 90deg.
bezierArc :: Floating u 
          => u -> Radian -> Radian -> Point2 u -> CubicBezier u
bezierArc r ang1 ang2 pt = CubicBezier p0 p1 p2 p3 
  where
    theta = ang2 - ang1
    e     = r * fromRadian ((2 * sin (theta/2)) / (1+ 2* cos (theta/2))) 
    p0    = pt .+^ avec ang1 r
    p1    = p0 .+^ avec (ang1 + pi/2) e
    p2    = p3 .+^ avec (ang2 - pi/2) e
    p3    = pt .+^ avec ang2 r


-- | Make a circle from Bezier curves - @n@ is the number of 
-- subdivsions per quadrant.
bezierCircle :: (Fractional u, Floating u) 
             => Int -> u -> Point2 u -> [CubicBezier u]
bezierCircle n r pt = para phi [] $ subdivisions (n*4) (2*pi) where
   phi a (b:_,acc) = bezierArc r a b pt : acc
   phi _ (_,acc)   = acc 

  

--------------------------------------------------------------------------------
-- operations

curveToPath :: CubicBezier u -> Path u
curveToPath (CubicBezier p0 p1 p2 p3) = path p0 [curveTo p1 p2 p3]



curvesToPath :: [CubicBezier u] -> Path u
curvesToPath []                     = error $ "curvesToPath - empty list"
curvesToPath (CubicBezier p0 p1 p2 p3:cs) = 
   path p0 (curveTo p1 p2 p3 : map fn cs) where 
      fn (CubicBezier _ u v w) = curveTo u v w

--------------------------------------------------------------------------------

circlePath :: (Fractional u, Floating u) 
           => Int -> u -> Point2 u -> Path u
circlePath = curvesToPath `ooo` bezierCircle



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
fillPolygon = appro fill id (vertexPath . vertexList) 

strokePolygon :: (Stroke t, Num u, Ord u) => t -> Polygon u -> Primitive u
strokePolygon = appro cstroke id (vertexPath . vertexList) 

--------------------------------------------------------------------------------