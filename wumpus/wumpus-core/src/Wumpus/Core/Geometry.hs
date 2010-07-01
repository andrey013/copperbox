{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Geometry
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Objects and operations for 2D geometry.
-- 
-- Vector, point, affine frame, 3x3 matrix, and radian 
-- representations, plus a type family @DUnit@ for parameterizing
-- type classes with some /dimension/.
--
--------------------------------------------------------------------------------

module Wumpus.Core.Geometry 
  ( 
  -- * Type family 
    DUnit
  
  -- * Data types
  , Vec2(..)
  , DVec2
  , Point2(..)
  , DPoint2
  , Frame2(..)
  , DFrame2
  , Matrix3'3(..)
  , DMatrix3'3
  , Radian


  -- * Pointwise type class
  , Pointwise(..)

  -- * Matrix multiply type class
  , MatrixMult(..)

  -- * Vector operations
  , direction
  , hvec
  , vvec
  , avec
  , pvec
  , vangle

  -- * Point operations
  , zeroPt
  , minPt
  , maxPt
  , langle

  -- * Frame operations
  , ortho
  , displaceOrigin
  , pointInFrame
  , frame2Matrix
  , matrix2Frame
  , frameProduct
  , standardFrame

  -- * Matrix contruction
  , identityMatrix
  , scalingMatrix
  , translationMatrix
  , rotationMatrix
  , originatedRotationMatrix

  -- * matrix operations
  , invert
  , determinant
  , transpose

  -- * Radian operations
  , req
  , toRadian
  , fromRadian
  , d2r
  , r2d
  , circularModulo

  -- * Bezier curves
  , bezierArc
  , bezierCircle

  ) where

import Wumpus.Core.Utils ( PSUnit(..), oo )


import Data.AffineSpace
import Data.VectorSpace

import Text.PrettyPrint.Leijen hiding ( langle )

import Data.Function ( on )
import Data.Monoid


--------------------------------------------------------------------------------

-- | Some unit of dimension usually double.

type family DUnit a :: *



-- Datatypes 

-- | 2D Vector - both components are strict.
--
data Vec2 u = V2 !u !u
  deriving (Eq,Show)

type DVec2 = Vec2 Double

-- | 2D Point - both components are strict.
-- 
-- Note - Point2 derives Ord so it can be used as a key in 
-- Data.Map etc.
--
data Point2 u = P2 !u !u
  deriving (Eq,Ord,Show)

type DPoint2 = Point2 Double



-- | A two dimensional frame.
-- 
-- The components are the two basis vectors @e0@ and @e1@ and 
-- the origin @o@.
--
-- Typically these names for the elements will be used:
--
-- > Frame2 (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)
-- 

data Frame2 u = Frame2 (Vec2 u) (Vec2 u) (Point2 u)
  deriving (Eq,Show)

type DFrame2 = Frame2 Double



-- | 3x3 matrix, considered to be in row-major form.
-- 
-- > (M3'3 a b c
-- >       d e f
-- >       g h i)
--
-- For instance the rotation matrix is represented as
--
-- >  ( cos(a) -sin(a) 0
-- >    sin(a)  cos(a) 0  
-- >      0         0  1 )
--
-- This seems commplace in geometry texts, but PostScript 
-- represents the @current-transformation-matrix@  in 
-- column-major form.
--
-- The right-most column is considered to represent a
-- coordinate:
--
-- >  ( 1 0 x
-- >    0 1 y  
-- >    0 0 1 ) 
-- >
-- 
-- So a translation matrix representing the displacement in x 
-- of 40 and in y of 10 would be:
--
-- >  ( 1 0 40
-- >    0 1 10  
-- >    0 0 1  ) 
-- >
-- 


data Matrix3'3 u = M3'3 !u !u !u  !u !u !u  !u !u !u
  deriving (Eq)

type DMatrix3'3 = Matrix3'3 Double



-- | Radian is represented with a distinct type. 
-- Equality and ordering are approximate where the epsilon 
-- is 0.0001.
newtype Radian = Radian { getRadian :: Double }
  deriving (Num,Real,Fractional,Floating,RealFrac,RealFloat)


--------------------------------------------------------------------------------
-- Family instances

type instance DUnit (Point2 u)    = u
type instance DUnit (Vec2 u)      = u
type instance DUnit (Frame2 u)    = u
type instance DUnit (Matrix3'3 u) = u

--------------------------------------------------------------------------------
-- lifters / convertors

lift2Vec2 :: (u -> u -> u) -> Vec2 u -> Vec2 u -> Vec2 u
lift2Vec2 op (V2 x y) (V2 x' y') = V2 (x `op` x') (y `op` y')


lift2Matrix3'3 :: (u -> u -> u) -> Matrix3'3 u -> Matrix3'3 u -> Matrix3'3 u
lift2Matrix3'3 op (M3'3 a b c d e f g h i) (M3'3 m n o p q r s t u) = 
      M3'3 (a `op` m) (b `op` n) (c `op` o)  
           (d `op` p) (e `op` q) (f `op` r)  
           (g `op` s) (h `op` t) (i `op` u)



--------------------------------------------------------------------------------
-- instances

-- Functor

instance Functor Vec2 where
  fmap f (V2 a b) = V2 (f a) (f b)


instance Functor Point2 where
  fmap f (P2 a b) = P2 (f a) (f b)

instance Functor Matrix3'3 where
  fmap f (M3'3 m n o p q r s t u) = 
    M3'3 (f m) (f n) (f o) (f p) (f q) (f r) (f s) (f t) (f u)


-- Monoid

-- Vectors have a sensible Monoid instance as addition, points don't

instance Num u => Monoid (Vec2 u) where
  mempty  = V2 0 0
  mappend = lift2Vec2 (+) 


-- Affine frames also have a sensible Monoid instance

instance (Num u, InnerSpace (Vec2 u)) => Monoid (Frame2 u) where
  mempty = ortho zeroPt
  mappend = frameProduct


-- Show

instance Show u => Show (Matrix3'3 u) where
  show (M3'3 a b c d e f g h i) = "(M3'3 " ++ body ++ ")" where
    body = show [[a,b,c],[d,e,f],[g,h,i]]

-- Num

instance Num u => Num (Matrix3'3 u) where
  (+) = lift2Matrix3'3 (+) 
  (-) = lift2Matrix3'3 (-)

  (*) (M3'3 a b c d e f g h i) (M3'3 m n o p q r s t u) = 
      M3'3 (a*m+b*p+c*s) (a*n+b*q+c*t) (a*o+b*r+c*u) 
           (d*m+e*p+f*s) (d*n+e*q+f*t) (d*o+e*r+f*u) 
           (g*m+h*p+i*s) (g*n+h*q+i*t) (g*o+h*r+i*u) 
  
  abs    = fmap abs 
  negate = fmap negate
  signum = fmap signum
  fromInteger a = M3'3 a' a' a'  a' a' a'  a' a' a' where a' = fromInteger a 

-- Instances for Radian which are 'special'.

instance Show Radian where
  showsPrec i (Radian a) = showsPrec i a

instance Eq Radian where (==) = req

instance Ord Radian where
  compare a b | a `req` b = EQ
              | otherwise = getRadian a `compare` getRadian b

--------------------------------------------------------------------------------
-- Pretty printing

instance Pretty u => Pretty (Vec2 u) where
  pretty (V2 a b) = angles (char '|' <+> pretty a <+> pretty b <+> char '|')

instance Pretty u => Pretty (Point2 u) where
  pretty (P2 a b) = brackets (char '|' <+> pretty a <+> pretty b <+> char '|')

instance Pretty u => Pretty (Frame2 u) where
  pretty (Frame2 e0 e1 o) = braces $
        text "e0:" <> pretty e0
    <+> text "e1:" <> pretty e1
    <+> text "o:" <> pretty o

instance PSUnit u => Pretty (Matrix3'3 u) where
  pretty (M3'3 a b c  d e f  g h i) = 
      matline a b c <$> matline d e f <$> matline g h i
    where
      matline x y z = char '|' 
         <+> (hcat $ map (fill 12 . text . dtrunc) [x,y,z]) 
         <+> char '|'   


instance Pretty Radian where
  pretty (Radian d) = double d <> text ":rad"

--------------------------------------------------------------------------------
-- Vector space instances

instance Num u => AdditiveGroup (Vec2 u) where
  zeroV = V2 0 0 
  (^+^) = lift2Vec2 (+)  
  negateV = fmap negate 


instance Num u => VectorSpace (Vec2 u) where
  type Scalar (Vec2 u) = u
  s *^ v = fmap (s*) v


-- scalar (dot / inner) product via the class InnerSpace

instance (Num u, InnerSpace u, Scalar u ~ u) 
    => InnerSpace (Vec2 u) where
  (V2 a b) <.> (V2 a' b') = (a <.> a') ^+^ (b <.> b')


instance Num u => AffineSpace (Point2 u) where
  type Diff (Point2 u) = Vec2 u
  (P2 a b) .-. (P2 x y)   = V2 (a-x)  (b-y)
  (P2 a b) .+^ (V2 vx vy) = P2 (a+vx) (b+vy)


instance Num u => AdditiveGroup (Matrix3'3 u) where
  zeroV = fromInteger 0
  (^+^) = (+)
  negateV = negate


instance Num u => VectorSpace (Matrix3'3 u) where
  type Scalar (Matrix3'3 u) = u
  s *^ m = fmap (s*) m 

--------------------------------------------------------------------------------

-- | Pointwise is a Functor like type class, except that the 
-- container/element relationship is defined via an associated 
-- type rather than a type parameter. This means that applied 
-- function must be type preserving.
--
class Pointwise sh where
  type Pt sh :: *
  pointwise :: (Pt sh -> Pt sh) -> sh -> sh


instance Pointwise (a -> a) where
  type Pt (a->a) = a
  pointwise f pf = \a -> pf (f a)

instance Pointwise a => Pointwise [a] where 
  type Pt [a] = Pt a
  pointwise f pts = map (pointwise f) pts 

instance Pointwise (Vec2 u) where
  type Pt (Vec2 u) = Vec2 u
  pointwise f v = f v

instance Pointwise (Point2 u) where
  type Pt (Point2 u) = Point2 u
  pointwise f pt = f pt


--------------------------------------------------------------------------------
-- Matrix multiply

infixr 7 *# 

-- | Matrix multiplication - typically of points and vectors 
-- represented as homogeneous coordinates. 
--
class MatrixMult t where 
  (*#) :: DUnit t ~ u => Matrix3'3 u -> t -> t


instance Num u => MatrixMult (Vec2 u) where       
  (M3'3 a b c d e f _ _ _) *# (V2 m n) = V2 (a*m+b*n+c*0) (d*m+e*n+f*0)


instance Num u => MatrixMult (Point2 u) where
  (M3'3 a b c d e f _ _ _) *# (P2 m n) = P2 (a*m+b*n+c*1) (d*m+e*n+f*1)

--------------------------------------------------------------------------------
-- Vectors


-- | Direction of a vector - i.e. the counter-clockwise angle 
-- from the x-axis.
--
direction :: (Floating u, Real u) => Vec2 u -> Radian
direction (V2 x y) = langle (P2 0 0) (P2 x y)

-- | Construct a vector with horizontal displacement.
--
hvec :: Num u => u -> Vec2 u
hvec d = V2 d 0

-- | Construct a vector with vertical displacement.
--
vvec :: Num u => u -> Vec2 u
vvec d = V2 0 d

-- | Construct a vector from an angle and magnitude.
--
avec :: Floating u => Radian -> u -> Vec2 u
avec theta d = V2 x y where
  ang = fromRadian theta
  x   = d * cos ang
  y   = d * sin ang


-- | The vector between two points
--
-- > pvec = flip (.-.)
--
pvec :: Num u => Point2 u -> Point2 u -> Vec2 u
pvec = flip (.-.)

-- | Extract the angle between two vectors.
--
vangle :: (Floating u, Real u, InnerSpace (Vec2 u)) 
       => Vec2 u -> Vec2 u -> Radian
vangle u v = realToFrac $ acos $ (u <.> v) / (on (*) magnitude u v)

--------------------------------------------------------------------------------
-- Points

-- | Construct a point at 0 0.
--
zeroPt :: Num u => Point2 u
zeroPt = P2 0 0


-- | /Component-wise/ min on points.  
-- Standard 'min' and 'max' via Ord are defined lexographically
-- on pairs, e.g.:
-- 
-- > min (1,2) (2,1) = (1,2)
-- 
-- For Points we want the component-wise min and max, e.g:
--
-- > minPt (P2 1 2) (Pt 2 1) = Pt 1 1 
-- > maxPt (P2 1 2) (Pt 2 1) = Pt 2 2
-- 
minPt :: Ord u => Point2 u -> Point2 u -> Point2 u
minPt (P2 x y) (P2 x' y') = P2 (min x x') (min y y')

-- | /Component-wise/ max on points.  
--
-- > maxPt (P2 1 2) (Pt 2 1) = Pt 2 2
-- 
maxPt :: Ord u => Point2 u -> Point2 u -> Point2 u
maxPt (P2 x y) (P2 x' y') = P2 (max x x') (max y y')


-- | Calculate the counter-clockwise angle between two points 
-- and the x-axis.
--
langle :: (Floating u, Real u) => Point2 u -> Point2 u -> Radian
langle (P2 x1 y1) (P2 x2 y2) = step (x2 - x1) (y2 - y1)
  where
    -- north-east quadrant 
    step x y | pve x && pve y = toRadian $ atan (y/x)          
    
    -- north-west quadrant
    step x y | pve y          = pi     - (toRadian $ atan (y / abs x))

    -- south-east quadrant
    step x y | pve x          = (2*pi) - (toRadian $ atan (abs y / x)) 

    -- otherwise... south-west quadrant
    step x y                  = pi     + (toRadian $ atan (y/x))

    pve a = signum a >= 0



--------------------------------------------------------------------------------
-- Frame operations

-- | Create a frame with standard (orthonormal bases) at the 
-- supplied point.
--
ortho :: Num u => Point2 u -> Frame2 u
ortho o = Frame2 (V2 1 0) (V2 0 1) o

-- | Displace the origin of the frame by the supplied vector.
--
displaceOrigin :: Num u => Vec2 u -> Frame2 u -> Frame2 u
displaceOrigin v (Frame2 e0 e1 o) = Frame2 e0 e1 (o.+^v)

-- | \'World coordinate\' calculation of a point in the supplied
-- frame.
--
pointInFrame :: Num u => Point2 u -> Frame2 u -> Point2 u
pointInFrame (P2 x y) (Frame2 vx vy o) = (o .+^ (vx ^* x)) .+^ (vy ^* y)  

-- | Concatenate the elements of the frame as columns forming a
-- 3x3 matrix. Points and vectors are considered homogeneous 
-- coordinates - triples where the least element is either 0 
-- indicating a vector or 1 indicating a point:
--
-- > Frame (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)
-- 
-- becomes
--
-- > (M3'3 e0x e1x ox
-- >       e0y e1y oy
-- >        0   0   1  )
--

frame2Matrix :: Num u =>  Frame2 u -> Matrix3'3 u
frame2Matrix (Frame2 (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)) = 
    M3'3 e0x e1x ox  
         e0y e1y oy 
         0   0   1


-- | Interpret the matrix as columns forming a frame.
--
-- > (M3'3 e0x e1x ox
-- >       e0y e1y oy
-- >        0   0   1  )
--
-- becomes
--
-- > Frame (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)
-- 
matrix2Frame :: Matrix3'3 u -> Frame2 u
matrix2Frame (M3'3 e0x e1x ox 
                   e0y e1y oy
                   _   _   _ ) = Frame2 (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)


-- | /Multiplication/ of frames to form their product.
--
frameProduct :: (Num u, InnerSpace (Vec2 u)) 
             => Frame2 u -> Frame2 u -> Frame2 u
frameProduct = matrix2Frame `oo` on (*) frame2Matrix



-- | Is the origin at (0,0) and are the basis vectors orthogonal 
-- with unit length?
--
standardFrame :: Num u => Frame2 u -> Bool
standardFrame (Frame2 (V2 1 0) (V2 0 1) (P2 0 0)) = True
standardFrame _                                   = False


--------------------------------------------------------------------------------
-- Matrix construction

-- | Construct the identity matrix:
--
-- > (M3'3 1 0 0
-- >       0 1 0
-- >       0 0 1 )
--
identityMatrix :: Num u => Matrix3'3 u
identityMatrix = M3'3 1 0 0  
                      0 1 0  
                      0 0 1

-- Common transformation matrices (for 2d homogeneous coordinates)

-- | Construct a scaling matrix:
--
-- > (M3'3 sx 0  0
-- >       0  sy 0
-- >       0  0  1 )
--
scalingMatrix :: Num u => u -> u -> Matrix3'3 u
scalingMatrix sx sy = M3'3  sx 0  0   
                            0  sy 0   
                            0  0  1

-- | Construct a translation matrix:
--
-- > (M3'3 1  0  x
-- >       0  1  y
-- >       0  0  1 )
--
translationMatrix :: Num u => u -> u -> Matrix3'3 u
translationMatrix x y = M3'3 1 0 x  
                             0 1 y  
                             0 0 1

-- | Construct a rotation matrix:
--
-- > (M3'3 cos(a)  -sin(a)  x
-- >       sin(a)   cos(a)  y
-- >       0        0       1 )
--
rotationMatrix :: (Floating u, Real u) => Radian -> Matrix3'3 u
rotationMatrix a = M3'3 (cos ang) (negate $ sin ang) 0 
                        (sin ang) (cos ang)          0  
                        0         0                  1
  where ang = fromRadian a

-- No reflectionMatrix function
-- A reflection about the x-axis is a scale of 1 (-1)
-- A reflection about the y-axis is a scale of (-1) 1


-- | Construct a matrix for rotation about some /point/.
--
-- This is the product of three matrices: T R T^-1
-- 
-- (T being the translation matrix, R the rotation matrix and
-- T^-1 the inverse of the translation matrix).
--
originatedRotationMatrix :: (Floating u, Real u) 
                         => Radian -> (Point2 u) -> Matrix3'3 u
originatedRotationMatrix ang (P2 x y) = mT * (rotationMatrix ang) * mTinv
  where
    mT    = M3'3 1 0 x     
                 0 1 y     
                 0 0 1

    mTinv = M3'3 1 0 (-x)  
                 0 1 (-y)  
                 0 0   1



-- | Invert a matrix.
--
invert :: Fractional u => Matrix3'3 u -> Matrix3'3 u
invert m = (1 / determinant m) *^ adjoint m

-- | Determinant of a matrix.
--
determinant :: Num u => Matrix3'3 u -> u
determinant (M3'3 a b c d e f g h i) = a*e*i - a*f*h - b*d*i + b*f*g + c*d*h - c*e*g

-- | Transpose a matrix.
--
transpose :: Matrix3'3 u -> Matrix3'3 u
transpose (M3'3 a b c 
                d e f 
                g h i) = M3'3 a d g  
                              b e h  
                              c f i

-- Helpers

adjoint :: Num u => Matrix3'3 u -> Matrix3'3 u
adjoint = transpose . cofactor . mofm


cofactor :: Num u => Matrix3'3 u -> Matrix3'3 u
cofactor (M3'3 a b c  
               d e f  
               g h i) = M3'3   a  (-b)   c
                             (-d)   e  (-f)
                               g  (-h)   i

mofm :: Num u => Matrix3'3 u -> Matrix3'3 u
mofm (M3'3 a b c  
               d e f  
               g h i)  = M3'3 m11 m12 m13  
                              m21 m22 m23 
                              m31 m32 m33
  where  
    m11 = (e*i) - (f*h)
    m12 = (d*i) - (f*g)
    m13 = (d*h) - (e*g)
    m21 = (b*i) - (c*h)
    m22 = (a*i) - (c*g)
    m23 = (a*h) - (b*g)
    m31 = (b*f) - (c*e)
    m32 = (a*f) - (c*d)
    m33 = (a*e) - (b*d)



--------------------------------------------------------------------------------
-- Radians


-- | The epislion used for floating point equality on radians.
radian_epsilon :: Double
radian_epsilon = 0.0001

-- | Equality on radians, this is the operation used for (==) in
-- Radian\'s Eq instance.
req :: Radian -> Radian -> Bool
req a b = (fromRadian $ abs (a-b)) < radian_epsilon



-- | Convert to radians.
toRadian :: Real a => a -> Radian 
toRadian = Radian . realToFrac


-- | Convert from radians.
fromRadian :: Fractional a => Radian -> a
fromRadian = realToFrac . getRadian


-- | Degrees to radians.
d2r :: (Floating a, Real a) => a -> Radian
d2r = Radian . realToFrac . (*) (pi/180)

-- | Radians to degrees.
r2d :: (Floating a, Real a) => Radian -> a
r2d = (*) (180/pi) . fromRadian


-- | Modulate a (positive) angle to be in the range 0..2*pi
--
circularModulo :: Radian -> Radian
circularModulo r = d2r $ dec + (fromIntegral $ i `mod` 360)
  where
    i       :: Integer
    dec     :: Double
    (i,dec) = properFraction $ r2d r


--------------------------------------------------------------------------------
-- Bezier curves

-- | Create an arc - this construction is the analogue of 
-- PostScript\'s @arc@ command, but the arc is created as a 
-- Bezier curve so it should span less than 90deg.
--
-- CAVEAT - ang2 must be greater than ang1 
--
bezierArc :: Floating u 
          => u -> Radian -> Radian -> Point2 u 
          -> (Point2 u, Point2 u, Point2 u, Point2 u)
bezierArc r ang1 ang2 pt = (p0,p1,p2,p3)
  where
    theta = ang2 - ang1
    e     = r * fromRadian ((2 * sin (theta/2)) / (1+ 2* cos (theta/2))) 
    p0    = pt .+^ avec ang1 r
    p1    = p0 .+^ avec (ang1 + pi/2) e
    p2    = p3 .+^ avec (ang2 - pi/2) e
    p3    = pt .+^ avec ang2 r


-- | Make a circle from Bezier curves - @n@ is the number of 
-- subdivsions per quadrant.
--
bezierCircle :: (Fractional u, Floating u) 
             => Int -> u -> Point2 u -> [Point2 u]
bezierCircle n radius pt = start $ subdivisions (n*4) (2*pi)
  where
    start (a:b:xs) = s : cp1 : cp2 : e : rest (b:xs)
      where (s,cp1,cp2,e) = bezierArc radius a b pt
                     
    start _        = [] 

    rest (a:b:xs)  = cp1 : cp2 : e : rest (b:xs)
      where (_,cp1,cp2,e) = bezierArc radius a b pt 

    rest _         = [] 

    subdivisions i a = 0 : take i (iterate (+one) one) 
      where  one  = a / fromIntegral i


