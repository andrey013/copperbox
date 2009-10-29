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
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- 2D geometry
--------------------------------------------------------------------------------

module Wumpus.Core.Geometry where

import Wumpus.Core.Utils ( dtrunc )

import Data.FunctionExtras

import Data.AffineSpace
import Data.VectorSpace

import Text.PrettyPrint.Leijen

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
data Frame2 a = Frame2 (Vec2 a) (Vec2 a) (Point2 a)
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
-- This is congruent with the form presented in Santos -  
-- Example 45, page 17 extended to 3x3. 
--
-- ref. David A. Santos /Multivariable and Vector Calculus/,
-- July 17, 2008 Version.
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


-- Affine frames also have a sensible Monoid instance

instance (Num a, InnerSpace (Vec2 a)) => Monoid (Frame2 a) where
  mempty = ortho zeroPt
  mappend = frameProduct





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
-- Pretty printing

instance Pretty a => Pretty (Vec2 a) where
  pretty (V2 a b) = angles (char '|' <+> pretty a <+> pretty b <+> char '|')

instance Pretty a => Pretty (Point2 a) where
  pretty (P2 a b) = brackets (char '|' <+> pretty a <+> pretty b <+> char '|')

instance Pretty a => Pretty (Frame2 a) where
  pretty (Frame2 e0 e1 o) = braces $
        text "e0:" <> pretty e0
    <+> text "e1:" <> pretty e1
    <+> text "o:" <> pretty o

instance Real a => Pretty (Matrix3'3 a) where
  pretty (M3'3 a b c  d e f  g h i) = 
      matline a b c <$> matline d e f <$> matline g h i
    where
      matline x y z = char '|' 
         <+> (hcat $ map (fill 12 . text . dtrunc . realToFrac) [x,y,z]) 
         <+> char '|'   


instance Pretty Radian where
  pretty (Radian d) = double d <> text ":rad"

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
ortho o = Frame2 (V2 1 0) (V2 0 1) o

displaceOrigin :: Num a => Vec2 a -> Frame2 a -> Frame2 a
displaceOrigin v (Frame2 e0 e1 o) = Frame2 e0 e1 (o.+^v)

coord :: Num a => Frame2 a -> Point2 a -> Point2 a
coord (Frame2 e0 e1 o) (P2 x y) = (o .+^ (x *^ e0)) .+^ (y *^ e1)


pointInFrame :: Num a => Point2 a -> Frame2 a -> Point2 a
pointInFrame (P2 x y) (Frame2 vx vy o) = (o .+^ (vx ^* x)) .+^ (vy ^* y)  

frame2Matrix :: Num a =>  Frame2 a -> Matrix3'3 a
frame2Matrix (Frame2 (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)) = 
    M3'3 e0x e1x ox  
         e0y e1y oy 
         0   0   1

matrix2Frame :: Matrix3'3 a -> Frame2 a
matrix2Frame (M3'3 e0x e1x ox 
                   e0y e1y oy
                   _   _   _ ) = Frame2 (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)


frameProduct :: (Num a, InnerSpace (Vec2 a)) => Frame2 a -> Frame2 a -> Frame2 a
frameProduct = matrix2Frame `oo` twine (*) frame2Matrix frame2Matrix



-- | Is the origin at (0,0) and are the basis vectors orthogonal 
-- with unit length?
standardFrame :: Num a => Frame2 a -> Bool
standardFrame (Frame2 (V2 1 0) (V2 0 1) (P2 0 0)) = True
standardFrame _                                   = False


--------------------------------------------------------------------------------
-- Matrix construction

identityMatrix :: Num a => Matrix3'3 a
identityMatrix = M3'3 1 0 0  
                      0 1 0  
                      0 0 1

-- Common transformation matrices (for 2d homogeneous coordinates)

scalingMatrix :: Num a => a -> a -> Matrix3'3 a
scalingMatrix sx sy = M3'3  sx 0  0   
                            0  sy 0   
                            0  0  1

translationMatrix :: Num a => a -> a -> Matrix3'3 a
translationMatrix x y = M3'3 1 0 x  
                             0 1 y  
                             0 0 1

rotationMatrix :: (Floating a, Real a) => Radian -> Matrix3'3 a
rotationMatrix a = M3'3 (cos ang) (negate $ sin ang) 0 
                        (sin ang) (cos ang)          0  
                        0         0                  1
  where ang = fromRadian a

-- No reflectionMatrix function
-- A reflection about the x-axis is a scale of 1 (-1)
-- A reflection about the y-axis is a scale of (-1) 1


-- Rotation about some /point/.
originatedRotationMatrix :: (Floating a, Real a) 
                         => Radian -> (Point2 a) -> Matrix3'3 a
originatedRotationMatrix ang (P2 x y) = mT * (rotationMatrix ang) * mTinv
  where
    mT    = M3'3 1 0 x     
                 0 1 y     
                 0 0 1

    mTinv = M3'3 1 0 (-x)  
                 0 1 (-y)  
                 0 0   1



-- inversion

invert :: Fractional a => Matrix3'3 a -> Matrix3'3 a 
invert m = (1 / determinant m) *^ adjoint m

determinant :: Num a => Matrix3'3 a -> a
determinant (M3'3 a b c d e f g h i) = a*e*i - a*f*h - b*d*i + b*f*g + c*d*h - c*e*g

transpose :: Matrix3'3 a -> Matrix3'3 a
transpose (M3'3 a b c 
                d e f 
                g h i) = M3'3 a d g  
                              b e h  
                              c f i

adjoint :: Num a => Matrix3'3 a -> Matrix3'3 a 
adjoint = transpose . cofactor . mofm

mofm :: Num a => Matrix3'3 a -> Matrix3'3 a
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


cofactor :: Num a => Matrix3'3 a -> Matrix3'3 a
cofactor (M3'3 a b c  
               d e f  
               g h i) = M3'3   a  (-b)   c
                             (-d)   e  (-f)
                               g  (-h)   i


--------------------------------------------------------------------------------
-- Affine transformations 


class Rotate t where
  rotate :: Radian -> t -> t


instance (Floating a, Real a) => Rotate (Point2 a) where
  rotate a = ((rotationMatrix a) *#)

instance (Floating a, Real a) => Rotate (Vec2 a) where
  rotate a = ((rotationMatrix a) *#)


class RotateAbout t where
  type RotateAboutUnit t
  rotateAbout :: Radian -> Point2 (RotateAboutUnit t) -> t -> t 


instance (Floating a, Real a) => RotateAbout (Point2 a) where
  type RotateAboutUnit (Point2 a) = a
  rotateAbout a pt = ((originatedRotationMatrix a pt) *#) 


instance (Floating a, Real a) => RotateAbout (Vec2 a) where
  type RotateAboutUnit (Vec2 a) = a
  rotateAbout a pt = ((originatedRotationMatrix a pt) *#) 
  

rotate90 :: Rotate t => t -> t 
rotate90 = rotate (pi/2) 

rotate90About :: (RotateAbout t, RotateAboutUnit t ~ u) 
              => Point2 u -> t -> t 
rotate90About = rotateAbout (pi/2)


rotate30 :: Rotate t => t -> t 
rotate30 = rotate (pi/6) 

rotate30About :: (RotateAbout t, RotateAboutUnit t ~ u) 
              => Point2 u -> t -> t 
rotate30About = rotateAbout (pi/6)


rotate45 :: Rotate t => t -> t 
rotate45 = rotate (pi/4) 

rotate45About :: (RotateAbout t, RotateAboutUnit t ~ u) 
              => Point2 u -> t -> t 
rotate45About = rotateAbout (pi/4)


rotate60 :: Rotate t => t -> t 
rotate60 = rotate (2*pi/3) 

rotate60About :: (RotateAbout t, RotateAboutUnit t ~ u) 
              => Point2 u -> t -> t 
rotate60About = rotateAbout (2*pi/3)


rotate120 :: Rotate t => t -> t 
rotate120 = rotate (4*pi/3) 

rotate120About :: (RotateAbout t, RotateAboutUnit t ~ u) 
               => Point2 u -> t -> t 
rotate120About = rotateAbout (4*pi/3)


circular :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a, Rotate t) 
         => [t] -> [t]
circular xs = snd $ mapAccumR fn 0 xs 
  where
    fn ang a = (ang+1, rotate (2*ang*pi/len) a)
    len      = fromIntegral $ length xs



class Scale t where
  type ScaleUnit t
  scale :: ScaleUnit t -> ScaleUnit t -> t -> t

instance Num u => Scale (Point2 u) where
  type ScaleUnit (Point2 u) = u
  scale x y = ((scalingMatrix x y) *#) 

instance Num u => Scale (Vec2 u) where
  type ScaleUnit (Vec2 u) = u
  scale x y = ((scalingMatrix x y) *#) 


uniformScale :: (Scale t, ScaleUnit t ~ u) => u -> t -> t 
uniformScale a = scale a a 


reflectX :: (Num u, Scale t, ScaleUnit t ~ u) => t -> t
reflectX = scale (-1) 1

reflectY :: (Num u, Scale t, ScaleUnit t ~ u) => t -> t
reflectY = scale 1 (-1)


class Translate t where
  type TranslateUnit t
  translate :: TranslateUnit t -> TranslateUnit t -> t -> t

-- | translate @x@ @y@.
instance Num u => Translate (Point2 u) where
  type TranslateUnit (Point2 u) = u
  translate x y = ((translationMatrix x y) *#)

instance Num u => Translate (Vec2 u) where
  type TranslateUnit (Vec2 u) = u
  translate x y = ((translationMatrix x y) *#)

translateBy :: (Translate t, TranslateUnit t ~ u) => Vec2 u -> t -> t 
translateBy (V2 x y) = translate x y


--------------------------------------------------------------------------------
-- degrees / radians

-- | Degrees to radians.
d2r :: (Floating a, Real a) => a -> Radian
d2r = Radian . realToFrac . (*) (pi/180)

-- | Radians to degrees.
r2d :: (Floating a, Real a) => Radian -> a
r2d = (*) (180/pi) . fromRadian

