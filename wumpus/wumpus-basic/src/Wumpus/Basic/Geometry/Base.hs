{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Geometry.Base
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Base geometric types and operations.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Geometry.Base
  ( 

  -- * constants
    quarter_pi
  , half_pi
  , two_pi

  -- * 2x2 Matrix
  , Matrix2'2(..)
  , DMatrix2'2
  , identity2'2
  , det2'2
  , transpose2'2

  -- * Line 
  , Line(..)
  , inclinedLine
  
  -- * Line in equational form 
  , LineEquation(..)
  , DLineEquation
  , lineEquation
  , pointViaX
  , pointViaY
  , pointLineDistance

  -- * Line segment
  , LineSegment(..)
  , DLineSegment
  
  , rectangleLineSegments
  , polygonLineSegments

  -- * Cubic Bezier curves
  
  , BezierCurve(..)
  , DBezierCurve

  , bezierLength
  , subdivide
  , subdividet
  
  , bezierArcPoints
  , bezierMinorArc

  -- * Functions
  , affineComb
  , midpoint
  , lineAngle


  ) 
  where

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace






quarter_pi      :: Floating u => u
quarter_pi      = 0.25 * pi

half_pi         :: Floating u => u
half_pi         = 0.5 * pi

two_pi          :: Floating u => u
two_pi          = 2.0 * pi


--------------------------------------------------------------------------------
-- 2x2 matrix


-- | 2x2 matrix, considered to be in row-major form.
-- 
-- > (M2'2 a b
-- >       c d)
--
-- 

data Matrix2'2 u = M2'2 !u !u   !u !u
  deriving (Eq)

type instance DUnit (Matrix2'2 u) = u

type DMatrix2'2 = Matrix2'2 Double



instance Functor Matrix2'2 where
  fmap f (M2'2 a b  c d) = M2'2 (f a) (f b)  (f c) (f d)

instance Show u => Show (Matrix2'2 u) where
  show (M2'2 a b c d) = "(M2'2 " ++ body ++ ")" 
    where
      body = show [[a,b],[c,d]]

lift2Matrix2'2 :: (u -> u -> u) -> Matrix2'2 u -> Matrix2'2 u -> Matrix2'2 u
lift2Matrix2'2 op (M2'2 a b c d) (M2'2 m n o p) = 
      M2'2 (a `op` m) (b `op` n) 
           (c `op` o) (d `op` p)

instance Num u => Num (Matrix2'2 u) where
  (+) = lift2Matrix2'2 (+) 
  (-) = lift2Matrix2'2 (-)

  (*) (M2'2 a b c d) (M2'2 m n o p) = 
      M2'2 (a*m + b*o) (a*n + b*p)  
           (c*m + d*o) (c*n + d*p) 
  
  abs    = fmap abs 
  negate = fmap negate
  signum = fmap signum
  fromInteger a = M2'2 a' a'  a' a' where a' = fromInteger a


-- | Construct the identity 2x2 matrix:
--
-- > (M2'2 1 0 
-- >       0 1 )
--
identity2'2 :: Num u => Matrix2'2 u
identity2'2 = M2'2  1 0  
                    0 1


-- | Determinant of a 2x2 matrix.
--
det2'2 :: Num u => Matrix2'2 u -> u
det2'2 (M2'2 a b c d) = a*d - b*c


-- | Transpose a 2x2 matrix.
--
transpose2'2 :: Matrix2'2 u -> Matrix2'2 u
transpose2'2 (M2'2 a b 
                   c d) = M2'2 a c
                               b d


--------------------------------------------------------------------------------

-- | Infinite line represented by two points.
--
data Line u = Line (Point2 u) (Point2 u)
  deriving (Eq,Show)

type instance DUnit (Line u) = u



-- | 'inclinedLine' : @ point * ang -> Line @
--
-- Make an infinite line passing through the supplied point 
-- inclined by @ang@.
--
inclinedLine :: Floating u => Point2 u -> Radian -> Line u
inclinedLine radial_ogin ang = Line radial_ogin (radial_ogin .+^ avec ang 100)





--------------------------------------------------------------------------------

-- | Line in equational form, i.e. @Ax + By + C = 0@.
--
data LineEquation u = LineEquation 
      { line_eqn_A :: !u
      , line_eqn_B :: !u
      , line_eqn_C :: !u 
      }
  deriving (Eq,Show)

type instance DUnit (LineEquation u) = u


type DLineEquation = LineEquation Double


-- | 'lineEquation' : @ point1 * point2 -> LineEquation @
-- 
-- Construct a line in equational form bisecting the supplied 
-- points.
--
lineEquation :: Num u => Point2 u -> Point2 u -> LineEquation u
lineEquation (P2 x1 y1) (P2 x2 y2) = LineEquation a b c 
  where
    a = y1 - y2
    b = x2 - x1
    c = (x1*y2) - (x2*y1)

-- | 'pointViaX' : @ x * line_equation -> Point @
--
-- Calculate the point on the line for the supplied @x@ value.
-- 
pointViaX :: Fractional u => u -> LineEquation u -> Point2 u
pointViaX x (LineEquation a b c) = P2 x y
  where
    y = ((a*x) + c) / (-b)


-- | 'pointViaY' : @ y * line_equation -> Point @
--
-- Calculate the point on the line for the supplied @y@ value.
-- 
pointViaY :: Fractional u => u -> LineEquation u -> Point2 u
pointViaY y (LineEquation a b c) = P2 x y
  where
    x = ((b*y) + c) / (-a)


-- | 'pointLineDistance' : @ point -> line -> Distance @
--
-- Find the distance from a point to a line in equational form
-- using this formula:
-- 
-- > P(u,v) 
-- > L: Ax + By + C = 0
-- >
-- > (A*u) + (B*v) + C 
-- > -----------------
-- > sqrt $ (A^2) +(B^2)
--
-- A positive distance indicates the point is above the line, 
-- negative indicates below.
--
pointLineDistance :: Floating u => Point2 u -> LineEquation u -> u
pointLineDistance (P2 u v) (LineEquation a b c) = 
    ((a*u) + (b*v) + c) / base
  where
    base = sqrt $ (a^two) + (b^two)
    two  :: Integer
    two  = 2

--------------------------------------------------------------------------------

data LineSegment u = LineSegment (Point2 u) (Point2 u)
  deriving (Eq,Ord,Show)

type instance DUnit (LineSegment u) = u


type DLineSegment = LineSegment Double




-- | 'rectangleLineSegments' : @ half_width * half_height -> [LineSegment] @
--
-- Compute the line segments of a rectangle.
--
rectangleLineSegments :: Num u => u -> u -> Point2 u -> [LineSegment u]
rectangleLineSegments hw hh ctr = 
    [ LineSegment br tr, LineSegment tr tl, LineSegment tl bl
    , LineSegment bl br 
    ]
  where
    br = ctr .+^ vec hw    (-hh)
    tr = ctr .+^ vec hw    hh
    tl = ctr .+^ vec (-hw) hh
    bl = ctr .+^ vec (-hw) (-hh)


-- | 'polygonLineSegments' : @ [point] -> [LineSegment] @
--
-- Build the line segments of a polygon fome a list of 
-- its vertices.
--
polygonLineSegments :: [Point2 u] -> [LineSegment u]
polygonLineSegments []     = []
polygonLineSegments (x:xs) = step x xs 
  where
    step a []        = [LineSegment a x]
    step a (b:bs)    = (LineSegment a b) : step b bs




--------------------------------------------------------------------------------
-- Bezier curves


-- | A Strict cubic Bezier curve.
--
data BezierCurve u = BezierCurve !(Point2 u) !(Point2 u) !(Point2 u) !(Point2 u)
  deriving (Eq,Ord,Show)

type instance DUnit (BezierCurve u) = u


type DBezierCurve = BezierCurve Double




-- | 'bezierLength' : @ start_point * control_1 * control_2 * 
--        end_point -> Length @ 
--
-- Find the length of a Bezier curve. The result is an 
-- approximation, with the /tolerance/ is 0.1 of a point. This
-- seems good enough for drawing (potentially the tolerance could 
-- be larger still). 
--
-- The result is found through repeated subdivision so the 
-- calculation is potentially costly.
--
bezierLength :: (Floating u, Ord u, Tolerance u)
             => BezierCurve u -> u
bezierLength = gravesenLength length_tolerance 



-- | Jens Gravesen\'s bezier arc-length approximation. 
--
-- Note this implementation is parametrized on error tolerance.
--
gravesenLength :: (Floating u, Ord u) => u -> BezierCurve u -> u
gravesenLength err_tol crv = step crv 
  where
    step c = let l1 = ctrlPolyLength c
                 l0 = cordLength c
             in if   l1-l0 > err_tol
                then let (a,b) = subdivide c in step a + step b
                else 0.5*l0 + 0.5*l1

-- | Length of the tree lines spanning the control points.
--
ctrlPolyLength :: Floating u => BezierCurve u -> u
ctrlPolyLength (BezierCurve p0 p1 p2 p3) = len p0 p1 + len p1 p2 + len p2 p3
  where
    len pa pb = vlength $ pvec pa pb


-- | Length of the cord - start point to end point.
--
cordLength :: Floating u => BezierCurve u -> u
cordLength (BezierCurve p0 _ _ p3) = vlength $ pvec p0 p3




-- | Curve subdivision via de Casteljau\'s algorithm.
--
subdivide :: Fractional u 
          => BezierCurve u -> (BezierCurve u, BezierCurve u)
subdivide (BezierCurve p0 p1 p2 p3) =
    (BezierCurve p0 p01 p012 p0123, BezierCurve p0123 p123 p23 p3)
  where
    p01   = midpoint p0    p1
    p12   = midpoint p1    p2
    p23   = midpoint p2    p3
    p012  = midpoint p01   p12
    p123  = midpoint p12   p23
    p0123 = midpoint p012  p123

-- | subdivide with an affine weight along the line...
--
subdividet :: Real u
           => u -> BezierCurve u -> (BezierCurve u, BezierCurve u)
subdividet t (BezierCurve p0 p1 p2 p3) = 
    (BezierCurve p0 p01 p012 p0123, BezierCurve p0123 p123 p23 p3)
  where
    p01   = affineComb t p0    p1
    p12   = affineComb t p1    p2
    p23   = affineComb t p2    p3
    p012  = affineComb t p01   p12
    p123  = affineComb t p12   p23
    p0123 = affineComb t p012  p123




kappa :: Floating u => u
kappa = 4 * ((sqrt 2 - 1) / 3)



-- | 'bezierArcPoints' : @ apex_angle * radius * rotation * center -> [Point] @
--
-- > ang should be in the range 0 < ang < 360deg.
--
-- > if   0 < ang <=  90 returns 4 points
-- > if  90 < ang <= 180 returns 7 points
-- > if 180 < ang <= 270 returns 10 points
-- > if 270 < ang <  360 returns 13 points
--
bezierArcPoints ::  Floating u 
                => Radian -> u -> Radian -> Point2 u -> [Point2 u]
bezierArcPoints ang radius theta pt = go (circularModulo ang)
  where
    go a | a <= half_pi = wedge1 a
         | a <= pi      = wedge2 (a/2)
         | a <= 1.5*pi  = wedge3 (a/3)
         | otherwise    = wedge4 (a/4)
    
    wedge1 a = 
      let (BezierCurve p0 p1 p2 p3) = bezierMinorArc a radius theta pt
      in [p0,p1,p2,p3]

    wedge2 a = 
      let (BezierCurve  p0 p1 p2 p3) = bezierMinorArc a radius theta pt
          (BezierCurve _   p4 p5 p6) = bezierMinorArc a radius (theta+a) pt
      in [ p0,p1,p2,p3, p4,p5,p6 ] 

    wedge3 a = 
      let (BezierCurve p0 p1 p2 p3) = bezierMinorArc a radius theta pt
          (BezierCurve _  p4 p5 p6) = bezierMinorArc a radius (theta+a) pt
          (BezierCurve _  p7 p8 p9) = bezierMinorArc a radius (theta+a+a) pt
      in [ p0,p1,p2,p3, p4,p5,p6, p7, p8, p9 ] 
  
    wedge4 a = 
      let (BezierCurve p0 p1 p2 p3)    = bezierMinorArc a radius theta pt
          (BezierCurve _  p4 p5 p6)    = bezierMinorArc a radius (theta+a) pt
          (BezierCurve _  p7 p8 p9)    = bezierMinorArc a radius (theta+a+a) pt
          (BezierCurve _  p10 p11 p12) = bezierMinorArc a radius (theta+a+a+a) pt
      in [ p0,p1,p2,p3, p4,p5,p6, p7,p8,p9, p10,p11, p12 ] 


-- | 'bezierMinorArc' : @ apex_angle * radius * rotation * center -> BezierCurve @
--
-- > ang should be in the range 0 < ang <= 90deg.
--
bezierMinorArc :: Floating u 
               => Radian -> u -> Radian -> Point2 u -> BezierCurve u
bezierMinorArc ang radius theta pt = BezierCurve p0 c1 c2 p3
  where
    kfactor = fromRadian $ ang / (0.5*pi)
    rl      = kfactor * radius * kappa
    totang  = circularModulo $ ang + theta

    p0      = dispParallel radius theta pt
    c1      = dispPerpendicular rl theta p0
    c2      = dispPerpendicular (-rl) totang p3
    p3      = dispParallel radius totang pt


--------------------------------------------------------------------------------
--


-- | Affine combination...
--
affineComb :: Real u => u -> Point2 u -> Point2 u -> Point2 u
affineComb t p1 p2 = p1 .+^ t *^ (p2 .-. p1)


-- | 'midpoint' : @ start_point * end_point -> Midpoint @
-- 
-- Mid-point on the line formed between the two supplied points.
--
midpoint :: Fractional u => Point2 u -> Point2 u -> Point2 u
midpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0



-- | 'lineAngle' : @ start_point * end_point -> Angle @
--
-- Calculate the counter-clockwise angle between the line formed 
-- by the two points and the horizontal plane.
--
lineAngle :: (Floating u, Real u) => Point2 u -> Point2 u -> Radian
lineAngle (P2 x1 y1) (P2 x2 y2) = step (x2 - x1) (y2 - y1)
  where
    -- north-east quadrant 
    step x y | pve x && pve y = toRadian $ atan (y/x)          
    
    -- north-west quadrant
    step x y | pve y          = pi     - (toRadian $ atan (y / abs x))

    -- south-east quadrant
    step x y | pve x          = (2*pi) - (toRadian $ atan (abs y / x)) 

    -- otherwise... south-west quadrant
    step x y                  = pi     + (toRadian $ atan (y/x))

    pve a                     = signum a >= 0

-- Ideally this would be in Geometry.Quadrant.
-- And surely there is a simpler formulation...


