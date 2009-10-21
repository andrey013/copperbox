{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Alt.Geometry
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- 2D geometry
--------------------------------------------------------------------------------

module Wumpus.Alt.Geometry where


import Data.AffineSpace
import Data.VectorSpace

import Data.List ( mapAccumR )
import Data.Monoid


--------------------------------------------------------------------------------

-- Datatypes 

-- Vectors

data Vec2 a = V2 !a !a
  deriving (Eq,Show)

type DVec2 = Vec2 Double

-- Points

data Point2 a = P2 !a !a
  deriving (Eq,Show)

type DPoint2 = Point2 Double



-- | Two dimensional frame.
data Frame2 a = Frame2 (Point2 a) (Vec2 a) (Vec2 a) 
  deriving (Eq,Show)

type DFrame2 = Frame2 Double



-- 3x3 matrix
data Matrix3'3 a = M3'3 !a !a !a  !a !a !a  !a !a !a
  deriving (Eq)

type DMatrix3'3 = Matrix3'3 Double


-- Radian numeric type

radian_epsilon :: Double
radian_epsilon = 0.0001

-- | Radian is represented with a distinct type. 
-- Equality and ordering are approximate where the epsilon is 0.0001.
newtype Radian = Radian { getRadian :: Double }
  deriving (Num,Real,Fractional,Floating,RealFrac,RealFloat)


--------------------------------------------------------------------------------
-- lifters / convertors

lift2Vec2 :: (a -> a -> a) -> Vec2 a -> Vec2 a -> Vec2 a
lift2Vec2 op (V2 x y) (V2 x' y') = V2 (x `op` x') (y `op` y')


lift2Matrix3'3 :: (a -> a -> a) -> Matrix3'3 a -> Matrix3'3 a -> Matrix3'3 a
lift2Matrix3'3 op (M3'3 a b c d e f g h i) (M3'3 m n o p q r s t u) = 
      M3'3 (a `op` m) (b `op` n) (c `op` o)  
           (d `op` p) (e `op` q) (f `op` r)  
           (g `op` s) (h `op` t) (i `op` u)


-- Radian construction
toRadian :: Real a => a -> Radian 
toRadian = Radian . realToFrac


-- Radian extraction 
fromRadian :: Fractional a => Radian -> a
fromRadian = realToFrac . getRadian


--------------------------------------------------------------------------------
-- instances


instance Functor Vec2 where
  fmap f (V2 a b) = V2 (f a) (f b)


instance Functor Point2 where
  fmap f (P2 a b) = P2 (f a) (f b)

instance Functor Matrix3'3 where
  fmap f (M3'3 m n o p q r s t u) = 
    M3'3 (f m) (f n) (f o) (f p) (f q) (f r) (f s) (f t) (f u)


-- Vectors have a sensible Monoid instance as addition, points don't



instance Num a => Monoid (Vec2 a) where
  mempty  = V2 0 0
  mappend = lift2Vec2 (+) 


instance Show a => Show (Matrix3'3 a) where
  show (M3'3 a b c d e f g h i) = "(M3'3 " ++ body ++ ")" where
    body = show [[a,b,c],[d,e,f],[g,h,i]]

instance Num a => Num (Matrix3'3 a) where
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

-- Radians

instance Show Radian where
  showsPrec i (Radian a) = showsPrec i a

req :: Radian -> Radian -> Bool
req a b = (fromRadian $ abs (a-b)) < radian_epsilon


instance Eq Radian where (==) = req

instance Ord Radian where
  compare a b | a `req` b = EQ
              | otherwise = getRadian a `compare` getRadian b


--------------------------------------------------------------------------------
-- Vector space and related instances

instance Num a => AdditiveGroup (Vec2 a) where
  zeroV = V2 0 0 
  (^+^) = lift2Vec2 (+)  
  negateV = fmap negate 


instance Num a => VectorSpace (Vec2 a) where
  type Scalar (Vec2 a) = a
  s *^ v = fmap (s*) v


-- scalar (dot / inner) product via the class InnerSpace

instance (Num a, InnerSpace a, Scalar a ~ a) 
    => InnerSpace (Vec2 a) where
  (V2 a b) <.> (V2 a' b') = (a <.> a') ^+^ (b <.> b')


instance Num a => AffineSpace (Point2 a) where
  type Diff (Point2 a) = Vec2 a
  (P2 a b) .-. (P2 x y)   = V2 (a-x)  (b-y)
  (P2 a b) .+^ (V2 vx vy) = P2 (a+vx) (b+vy)


instance Num a => AdditiveGroup (Matrix3'3 a) where
  zeroV = fromInteger 0
  (^+^) = (+)
  negateV = negate


instance Num a => VectorSpace (Matrix3'3 a) where
  type Scalar (Matrix3'3 a) = a
  s *^ m = fmap (s*) m 

--------------------------------------------------------------------------------

-- | Pointwise is a Functor like type class, except that the 
-- container/element relationship is defined by a type family 
-- rather than a type parameter. This means that applied function 
-- must be type preserving.


class Pointwise sh where
  type Pt sh :: *
  pointwise :: (Pt sh -> Pt sh) -> sh -> sh


instance Pointwise (a -> a) where
  type Pt (a->a) = a
  pointwise f pf = \a -> pf (f a)

instance Pointwise a => Pointwise [a] where 
  type Pt [a] = Pt a
  pointwise f pts = map (pointwise f) pts 

instance Pointwise (Vec2 a) where
  type Pt (Vec2 a) = Vec2 a
  pointwise f v = f v

instance Pointwise (Point2 a) where
  type Pt (Point2 a) = Point2 a
  pointwise f pt = f pt

--------------------------------------------------------------------------------
-- Matrix multiply

infixr 7 *# 

class MatrixMult mat t where 
  type MatrixParam t :: *
  (*#) :: MatrixParam t ~ a => mat a -> t -> t


-- Matrix multiplication of points and vectors as per homogeneous 
-- coordinates (we don't perform the /last three/ multiplications
-- as we throw the result away).
 
instance Num a => MatrixMult Matrix3'3 (Vec2 a) where
  type MatrixParam (Vec2 a) = a       
  (M3'3 a b c d e f _ _ _) *# (V2 m n) = V2 (a*m+b*n+c*0) (d*m+e*n+f*0)


instance Num a => MatrixMult Matrix3'3 (Point2 a) where
  type MatrixParam (Point2 a) = a
  (M3'3 a b c d e f _ _ _) *# (P2 m n) = P2 (a*m+b*n+c*1) (d*m+e*n+f*1)

--------------------------------------------------------------------------------
-- Vectors

hvec :: Num a => a -> Vec2 a
hvec d = V2 d 0

vvec :: Num a => a -> Vec2 a
vvec d = V2 0 d




--------------------------------------------------------------------------------
-- Points

zeroPt :: Num a => Point2 a
zeroPt = P2 0 0

langle :: (Floating u, Real u) => Point2 u -> Point2 u -> Radian
langle (P2 x y) (P2 x' y') = toRadian $ atan $ (y'-y) / (x'-x) 


--------------------------------------------------------------------------------
-- Frame operations

ortho :: Num a => Point2 a -> Frame2 a
ortho o = Frame2 o (V2 1 0) (V2 0 1)

displaceOrigin :: Num a => Vec2 a -> Frame2 a -> Frame2 a
displaceOrigin v (Frame2 o vx vy) = Frame2 (o.+^v) vx vy

coord :: Num a => Frame2 a -> Point2 a -> Point2 a
coord (Frame2 o e0 e1) (P2 x y) = (o .+^ (x *^ e0)) .+^ (y *^ e1)


pointInFrame :: Num a => Point2 a -> Frame2 a -> Point2 a
pointInFrame (P2 x y) (Frame2 o vx vy) = (o .+^ (vx ^* x)) .+^ (vy ^* y)  


--------------------------------------------------------------------------------
-- Matrix construction

identityMatrix :: Num a => Matrix3'3 a
identityMatrix = M3'3 1 0 0  0 1 0  0 0 1

-- Common transformation matrices (for 2d homogeneous coordinates)

scalingMatrix :: Num a => a -> a -> Matrix3'3 a
scalingMatrix sx sy = M3'3  sx 0 0   0 sy 0   0 0 1

translationMatrix :: Num a => a -> a -> Matrix3'3 a
translationMatrix x y = M3'3 1 0 x  0 1 y  0 0 1

rotationMatrix :: (Floating a, Real a) => Radian -> Matrix3'3 a
rotationMatrix a = M3'3 (cos ang) (- sin ang) 0 
                        (sin ang) (cos ang)   0  
                        0         0           1
  where ang = fromRadian a

-- No reflectionMatrix function
-- A reflection about the x-axis is a scale of 1 (-1)
-- A reflection about the y-axis is a scale of (-1) 1


-- Rotation about some /point/.
originatedRotationMatrix :: (Floating a, Real a) 
                         => Radian -> (Point2 a) -> Matrix3'3 a
originatedRotationMatrix ang (P2 x y) = mT * (rotationMatrix ang) * mTinv
  where
    mT    = M3'3 1 0 x     0 1 y     0 0 1
    mTinv = M3'3 1 0 (-x)  0 1 (-y)  0 0 1

rotate :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
       => Radian -> t -> t
rotate a = ((rotationMatrix a) *#)

rotateAbout :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
            => Radian -> Point2 a -> t -> t 
rotateAbout a pt = ((originatedRotationMatrix a pt) *#) 

rotate90 :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
         => t -> t 
rotate90 = rotate (pi/2) 

rotate90About :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
              => Point2 a -> t -> t 
rotate90About = rotateAbout (pi/2)


rotate30 :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
         => t -> t 
rotate30 = rotate (pi/6) 

rotate30About :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
              => Point2 a -> t -> t 
rotate30About = rotateAbout (pi/6)


rotate45 :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
         => t -> t 
rotate45 = rotate (pi/4) 

rotate45About :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
              => Point2 a -> t -> t 
rotate45About = rotateAbout (pi/4)


rotate60 :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
         => t -> t 
rotate60 = rotate (2*pi/3) 

rotate60About :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
              => Point2 a -> t -> t 
rotate60About = rotateAbout (2*pi/3)


rotate120 :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
          => t -> t 
rotate120 = rotate (4*pi/3) 

rotate120About :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
               => Point2 a -> t -> t 
rotate120About = rotateAbout (4*pi/3)


circular :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
         => [t] -> [t]
circular xs = snd $ mapAccumR fn 0 xs 
  where
    fn ang a = (ang+1, rotate (2*ang*pi/len) a)
    len      = fromIntegral $ length xs


scale :: (Num a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
      => a -> a -> t -> t 
scale x y = ((scalingMatrix x y) *#) 

uniformScale :: (Floating a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
             => a -> t -> t 
uniformScale a = scale a a 


-- | translate @x@ @y@.
translate :: (Num a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
          => a -> a -> t -> t 
translate x y = ((translationMatrix x y) *#)

translateBy :: (Num a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
            => Vec2 a -> t -> t 
translateBy (V2 x y) = translate x y

reflectX :: (Num a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
         => t -> t
reflectX = scale (-1) 1

reflectY :: (Num a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
         => t -> t
reflectY = scale 1 (-1)




