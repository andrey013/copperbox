{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Matrix
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Matrices
--
--------------------------------------------------------------------------------


module Wumpus.Core.Matrix 
  (
  -- * Matrix types
    Matrix2'2(..)
  , DMatrix2'2
  , Matrix3'3(..)
  , DMatrix3'3
  
  -- * Matrix multiply
  , MatrixMult(..)

  -- * Construct identity matrix
  , IdentityMatrix(..)

  -- * Transposition
  , Transpose(..)

  -- * Determinant
  , Determinant(..)
  , invertible

  -- * Square Matrices
  , SquareMatrix(..)

  -- * Indexed access
  , Indexical(..)

  -- * Elementary matrices
  , elementarySwapRows
  , elementaryReplace_i
  , elementaryReplace_i_j


  -- * Invert a matrix
  , Inverse(..)


  -- * Common transformation matrices (for 2D homogeneous coordinates)
  , scalingMatrix
  , translationMatrix
  , rotationMatrix
  , rotationMatrix'

  ) where

import Wumpus.Core.Radian

import Data.AdditiveGroup ()
import Data.VectorSpace

--------------------------------------------------------------------------------
-- Matrix types and standard instances

data Matrix2'2 a = M2'2 !a !a !a !a
  deriving (Eq)

type DMatrix2'2 = Matrix2'2 Double


data Matrix3'3 a = M3'3 !a !a !a  !a !a !a  !a !a !a
  deriving (Eq)

type DMatrix3'3 = Matrix3'3 Double


instance Show a => Show (Matrix2'2 a) where
  show (M2'2 a b c d) = "(M2'2 " ++ show [[a,b],[c,d]] ++ ")"  


instance Show a => Show (Matrix3'3 a) where
  show (M3'3 a b c d e f g h i) = "(M3'3 " ++ body ++ ")" where
    body = show [[a,b,c],[d,e,f],[g,h,i]]



instance Num a => Num (Matrix2'2 a) where
  (+) (M2'2 a b c d) (M2'2 e f g h) = M2'2 (a+e) (b+f) (c+g) (d+h)
  (-) (M2'2 a b c d) (M2'2 e f g h) = M2'2 (a-e) (b-f) (c-g) (d-h)
  (*) (M2'2 a b c d) (M2'2 e f g h) = M2'2 (a*e+b*g) (a*f+b*h) (c*e+d*g) (c*f+d*h)
  
  abs (M2'2 a b c d)      = M2'2 (abs a) (abs b) (abs c) (abs d)
  negate (M2'2 a b c d)   = M2'2 (negate a) (negate b) (negate c) (negate d)
  signum (M2'2 a b c d)   = M2'2 (signum a) (signum b) (signum c) (signum d)
  fromInteger a           = M2'2 (fromInteger a) (fromInteger a) 
                                 (fromInteger a) (fromInteger a)
  


instance Num a => Num (Matrix3'3 a) where
  (+) (M3'3 a b c d e f g h i) (M3'3 m n o p q r s t u) = 
      M3'3 (a+m) (b+n) (c+o)  (d+p) (e+q) (f+r)  (g+s) (h+t) (i+u)
  (-) (M3'3 a b c d e f g h i) (M3'3 m n o p q r s t u) = 
      M3'3 (a-m) (b-n) (c-o)  (d-p) (e-q) (f-r)  (g-s) (h-t) (i-u)


  (*) (M3'3 a b c d e f g h i) (M3'3 m n o p q r s t u) = 
      M3'3 (a*m+b*p+c*s) (a*n+b*q+c*t) (a*o+b*r+c*u) 
           (d*m+e*p+f*s) (d*n+e*q+f*t) (d*o+e*r+f*u) 
           (g*m+h*p+i*s) (g*n+h*q+i*t) (g*o+h*r+i*u) 
  
  abs (M3'3 a b c d e f g h i)      = M3'3 (abs a) (abs b) (abs c)  
                                           (abs d) (abs e) (abs f)  
                                           (abs g) (abs h) (abs i) 
  negate (M3'3 a b c d e f g h i)   = M3'3 (negate a) (negate b) (negate c) 
                                           (negate d) (negate e) (negate f)
                                           (negate g) (negate h) (negate i)
  signum (M3'3 a b c d e f g h i)   = M3'3 (signum a) (signum b) (signum c) 
                                           (signum d) (signum e) (signum f)
                                           (signum g) (signum h) (signum i)
  fromInteger a           = M3'3 a' a' a'  a' a' a'  a' a' a'
    where a'              = fromInteger a 


instance Functor Matrix2'2 where
  fmap fn (M2'2 a b c d) = M2'2 (fn a) (fn b)  (fn c) (fn d)

instance Functor Matrix3'3 where
  fmap fn (M3'3 a b c d e f g h i) =
    M3'3 (fn a) (fn b) (fn c)  (fn d) (fn e) (fn f)  (fn g) (fn h) (fn i)




instance Num a => AdditiveGroup (Matrix2'2 a) where
  zeroV = M2'2 0 0  0 0
  (^+^) = (+)
  negateV = negate


instance Num a => AdditiveGroup (Matrix3'3 a) where
  zeroV = M3'3 0 0 0   0 0 0   0 0 0
  (^+^) = (+)
  negateV = negate
 

instance (Num a, VectorSpace a) => VectorSpace (Matrix2'2 a) where
  type Scalar (Matrix2'2 a) = Scalar a
  s *^ (M2'2 a b  c d) = M2'2 (s*^a) (s*^b)  (s*^c) (s*^d)



instance (Num a, VectorSpace a) => VectorSpace (Matrix3'3 a) where
  type Scalar (Matrix3'3 a) = Scalar a
  s *^ (M3'3 a b c  d e f  g h i) = M3'3 (s*^a) (s*^b) (s*^c) 
                                         (s*^d) (s*^e) (s*^f)
                                         (s*^g) (s*^h) (s*^i)


--------------------------------------------------------------------------------
-- Matrix multiply

infixr 7 *# 

class MatrixMult t u where 
  (*#) :: Num a => t a -> u a -> u a


--------------------------------------------------------------------------------
-- Construct identity matrix

class IdentityMatrix t where
  identityMatrix :: Num a => t a 

instance IdentityMatrix Matrix2'2 where
  identityMatrix = M2'2 1 0 0 1

instance IdentityMatrix Matrix3'3 where
  identityMatrix = M3'3 1 0 0  0 1 0  0 0 1


--------------------------------------------------------------------------------
-- Transposition  

class Transpose t t' where
  transpose :: t a -> t' a

instance Transpose Matrix2'2 Matrix2'2 where
  transpose (M2'2 a b c d)= M2'2 a c b d

instance Transpose Matrix3'3 Matrix3'3 where
  transpose (M3'3 a b c d e f g h i) = M3'3 a d g  b e h  c f i

--------------------------------------------------------------------------------
-- Determinant

class Determinant t where
  det :: Num a => t a -> a

instance Determinant Matrix2'2 where 
  det (M2'2 a b c d) = a*d - b*c

instance Determinant Matrix3'3 where
  det (M3'3 a b c d e f g h i) = a*e*i - a*f*h - b*d*i + b*f*g + c*d*h - c*e*g


-- | A matrix is invertible if it\'s determininat is not zero.
invertible :: (Determinant t, Num a) => t a -> Bool
invertible = (/=) 0 . det



--------------------------------------------------------------------------------
-- Square matrices

-- | Operations and predicates on square matrices.
class SquareMatrix t where
  trace               :: Num a => t a -> a
  isDiagonal          :: Num a => t a -> Bool
  diagonals           :: t a -> [a]
  counterDiagonals    :: t a -> [a] 
  isScalarMatrix      :: Num a => t a -> Bool

instance SquareMatrix Matrix2'2 where 
  trace             (M2'2 a _  _ d) = a+d
  isDiagonal        (M2'2 _ b  c _) = b==0 && c==0
  diagonals         (M2'2 a _  _ d) = [a,d]  
  counterDiagonals  (M2'2 _ b  c _) = [b,c]
  isScalarMatrix  m@(M2'2 a _  _ d) = isDiagonal m && a==d

instance SquareMatrix Matrix3'3 where
  trace             (M3'3 a _ _  _ e _  _ _ i) = a+e+i
  isDiagonal        (M3'3 _ b c  d _ f  g h _) = b==0 && c==0 && d==0 && f==0 
                                                      && g==0 && h==0
  diagonals         (M3'3 a _ _  _ e _  _ _ i) = [a,e,i]
  counterDiagonals  (M3'3 _ _ c  _ e _  g _ _) = [c,e,g]
  isScalarMatrix  m@(M3'3 a _ _  _ e _  _ _ i) = isDiagonal m && a==e && e==i






--------------------------------------------------------------------------------
-- Indexed access to the matrix...

class Indexical t where
  toIndexical   :: t a -> t (Int,Int,a)
  fromIndexical :: t (Int,Int,a) -> t a
  atIx          :: Int -> Int -> t a -> a
 
-- third-of-three
toft :: (a,b,c) -> c
toft (_,_,c) = c


instance Indexical Matrix2'2 where
  toIndexical (M2'2 a b c d) = M2'2 (1,1,a) (1,2,b) 
                                    (2,1,c) (2,2,d)

  fromIndexical = fmap toft

  atIx 1 1 (M2'2 a _ _ _) = a
  atIx 1 2 (M2'2 _ b _ _) = b
  atIx 2 1 (M2'2 _ _ c _) = c
  atIx 2 2 (M2'2 _ _ _ d) = d
  atIx i j _              = error $ 
      "index " ++ show (i,j) ++ " out-of-bounds of a 2x2 matrix" 
   
instance Indexical Matrix3'3 where
  toIndexical (M3'3 a b c d e f g h i) = 
    M3'3 (1,1,a) (1,2,b) (1,3,c) 
         (2,1,d) (2,2,e) (2,3,f)
         (3,1,g) (3,2,h) (3,3,i)

  fromIndexical = fmap toft

  atIx 1 1 (M3'3 a _ _  _ _ _  _ _ _) = a
  atIx 1 2 (M3'3 _ b _  _ _ _  _ _ _) = b
  atIx 1 3 (M3'3 _ _ c  _ _ _  _ _ _) = c
  atIx 2 1 (M3'3 _ _ _  d _ _  _ _ _) = d
  atIx 2 2 (M3'3 _ _ _  _ e _  _ _ _) = e
  atIx 2 3 (M3'3 _ _ _  _ _ f  _ _ _) = f       
  atIx 3 1 (M3'3 _ _ _  _ _ _  g _ _) = g
  atIx 3 2 (M3'3 _ _ _  _ _ _  _ h _) = h
  atIx 3 3 (M3'3 _ _ _  _ _ _  _ _ i) = i       
  atIx i j _                          = error $ 
      "index " ++ show (i,j) ++ " out-of-bounds of a 3x3 matrix" 




--------------------------------------------------------------------------------
-- Elementary matrices

-- | swap rows
elementarySwapRows :: (Num a, Functor t, Indexical t, IdentityMatrix t) 
                => Int -> Int -> t a
elementarySwapRows i j = fmap fn . toIndexical $ identityMatrix 
  where fn (k,l,o) | k/=i && k/=j && l/=k = 0
                   | k/=i && k/=j && l==k = 1
                   | k==i && l/=j         = 0
                   | k==i && l==j         = 1
                   | k==j && l/=i         = 0
                   | k==j && l==i         = 1
                   | otherwise            = o 

elementaryReplace_i :: (Num a, Functor t, Indexical t, IdentityMatrix t) 
                      => Int -> a -> t a
elementaryReplace_i i a = fmap fn . toIndexical $ identityMatrix
  where fn (k,l,o) | k/=i && l/=k = 0
                   | k/=i && l==k = 1
                   | k==i && l==i = a
                   | otherwise    = o


elementaryReplace_i_j :: (Num a, Functor t, Indexical t, IdentityMatrix t) 
                      => Int -> Int -> a -> t a
elementaryReplace_i_j i j a = fmap fn . toIndexical $ identityMatrix
  where fn (k,l,o) | k/=j && l/=k         = 0
                   | k/=j && l==k         = 1
                   | k==j && l/=i && l/=j = 0
                   | k==j && l==j         = 1
                   | k==j && l==i         = a
                   | otherwise            = o


-------------------------------------------------------------------------------

class Inverse t where
  inverse ::  (Fractional a, VectorSpace a, a ~ Scalar a) => t a -> t a 

instance Inverse Matrix2'2 where
  inverse = inverse2'2

instance Inverse Matrix3'3 where
  inverse = inverse3'3

inverse2'2 :: (Fractional a, VectorSpace a, a ~ Scalar a) => Matrix2'2 a -> Matrix2'2 a 
inverse2'2 m@(M2'2 a b c d) = (1 / det m) *^ (M2'2 d (-b)  (-c) a)


inverse3'3 :: (Fractional a, VectorSpace a, a ~ Scalar a) => Matrix3'3 a -> Matrix3'3 a 
inverse3'3 m = (1 / det m) *^ adjoint3'3 m

adjoint3'3 :: Num a => Matrix3'3 a -> Matrix3'3 a 
adjoint3'3 = transpose . cofactor3'3 . mofm3'3

mofm3'3 :: Num a => Matrix3'3 a -> Matrix3'3 a
mofm3'3 (M3'3 a b c  d e f  g h i)  = M3'3 m11 m12 m13  m21 m22 m23 m31 m32 m33
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


cofactor3'3 :: Num a => Matrix3'3 a -> Matrix3'3 a
cofactor3'3 (M3'3 a b c  d e f  g h i) = M3'3 a  (-b) c
                                             (-d) e (-f)
                                             g  (-h) i


--------------------------------------------------------------------------------
-- Common transformation matrices (for 2d homogeneous coordinates)

scalingMatrix :: Num a => a -> a -> Matrix3'3 a
scalingMatrix sx sy = M3'3  sx 0 0   0 sy 0   0 0 1

translationMatrix :: Num a => a -> a -> Matrix3'3 a
translationMatrix x y = M3'3 1 0 x  0 1 y  0 0 1




rotationMatrix :: Floating a => Radian a -> Matrix3'3 a
rotationMatrix (Radian ang) = M3'3 (cos ang) (- sin ang) 0 
                                   (sin ang) (cos ang)   0  
                                   0         0           1

rotationMatrix' :: Floating a => Radian a -> a -> a -> Matrix3'3 a
rotationMatrix' ang x y = mT * (rotationMatrix ang) * mTinv
  where
    mT    = M3'3 1 0 x     0 1 y     0 0 1
    mTinv = M3'3 1 0 (-x)  0 1 (-y)  0 0 1

 
-- No reflectionMatrix function
-- A reflection about the x-axis is a scale of 1 (-1)
-- A reflection about the y-axis is a scale of (-1) 1
