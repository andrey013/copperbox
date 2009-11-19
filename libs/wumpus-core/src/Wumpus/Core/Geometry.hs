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
  , hvec
  , vvec
  , avec

  -- * Point operations
  , zeroPt
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

  ) where

import Wumpus.Core.Utils ( CMinMax(..), PSUnit(..) )

import Data.Aviary

import Data.AffineSpace
import Data.VectorSpace

import Text.PrettyPrint.Leijen hiding ( langle )

import Data.Function ( on )
import Data.Monoid


--------------------------------------------------------------------------------

-- | Some unit of dimension usually double.

type family DUnit a :: *



-- Datatypes 

-- Vectors

data Vec2 a = V2 !a !a
  deriving (Eq,Show)

type DVec2 = Vec2 Double

-- Points

data Point2 a = P2 !a !a
  deriving (Eq,Show)

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



-- | Radian is represented with a distinct type. 
-- Equality and ordering are approximate where the epsilon is 0.0001.
newtype Radian = Radian { getRadian :: Double }
  deriving (Num,Real,Fractional,Floating,RealFrac,RealFloat)


--------------------------------------------------------------------------------
-- Family instances

type instance DUnit (Point2 a)    = a
type instance DUnit (Vec2 a)      = a
type instance DUnit (Frame2 a)    = a
type instance DUnit (Matrix3'3 a) = a

--------------------------------------------------------------------------------
-- lifters / convertors

lift2Vec2 :: (a -> a -> a) -> Vec2 a -> Vec2 a -> Vec2 a
lift2Vec2 op (V2 x y) (V2 x' y') = V2 (x `op` x') (y `op` y')


lift2Matrix3'3 :: (a -> a -> a) -> Matrix3'3 a -> Matrix3'3 a -> Matrix3'3 a
lift2Matrix3'3 op (M3'3 a b c d e f g h i) (M3'3 m n o p q r s t u) = 
      M3'3 (a `op` m) (b `op` n) (c `op` o)  
           (d `op` p) (e `op` q) (f `op` r)  
           (g `op` s) (h `op` t) (i `op` u)



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

instance PSUnit a => Pretty (Matrix3'3 a) where
  pretty (M3'3 a b c  d e f  g h i) = 
      matline a b c <$> matline d e f <$> matline g h i
    where
      matline x y z = char '|' 
         <+> (hcat $ map (fill 12 . text . dtrunc) [x,y,z]) 
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

instance Ord a => CMinMax (Point2 a) where
  cmin (P2 x y) (P2 x' y') = P2 (min x x') (min y y')
  cmax (P2 x y) (P2 x' y') = P2 (max x x') (max y y')

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

-- | Construct a vector with horizontal displacement.
hvec :: Num a => a -> Vec2 a
hvec d = V2 d 0

-- | Construct a vector with vertical displacement.
vvec :: Num a => a -> Vec2 a
vvec d = V2 0 d

-- | Construct a vector from an angle and magnitude.
avec :: Floating a => Radian -> a -> Vec2 a
avec theta d = V2 x y where
  ang = fromRadian theta
  x   = d * cos ang
  y   = d * sin ang


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


pointInFrame :: Num a => Point2 a -> Frame2 a -> Point2 a
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

frame2Matrix :: Num a =>  Frame2 a -> Matrix3'3 a
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

matrix2Frame :: Matrix3'3 a -> Frame2 a
matrix2Frame (M3'3 e0x e1x ox 
                   e0y e1y oy
                   _   _   _ ) = Frame2 (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)


-- | /Multiplication/ of frames to form their product.
frameProduct :: (Num a, InnerSpace (Vec2 a)) => Frame2 a -> Frame2 a -> Frame2 a
frameProduct = matrix2Frame `oo` on (*) frame2Matrix



-- | Is the origin at (0,0) and are the basis vectors orthogonal 
-- with unit length?
standardFrame :: Num a => Frame2 a -> Bool
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

identityMatrix :: Num a => Matrix3'3 a
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


adjoint :: Num a => Matrix3'3 a -> Matrix3'3 a 
adjoint = transpose . cofactor . mofm

transpose :: Matrix3'3 a -> Matrix3'3 a
transpose (M3'3 a b c 
                d e f 
                g h i) = M3'3 a d g  
                              b e h  
                              c f i

cofactor :: Num a => Matrix3'3 a -> Matrix3'3 a
cofactor (M3'3 a b c  
               d e f  
               g h i) = M3'3   a  (-b)   c
                             (-d)   e  (-f)
                               g  (-h)   i

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



--------------------------------------------------------------------------------
-- Radians

-- Radian numeric type

radian_epsilon :: Double
radian_epsilon = 0.0001


req :: Radian -> Radian -> Bool
req a b = (fromRadian $ abs (a-b)) < radian_epsilon



-- Radian construction
toRadian :: Real a => a -> Radian 
toRadian = Radian . realToFrac


-- Radian extraction 
fromRadian :: Fractional a => Radian -> a
fromRadian = realToFrac . getRadian


-- | Degrees to radians.
d2r :: (Floating a, Real a) => a -> Radian
d2r = Radian . realToFrac . (*) (pi/180)

-- | Radians to degrees.
r2d :: (Floating a, Real a) => Radian -> a
r2d = (*) (180/pi) . fromRadian

