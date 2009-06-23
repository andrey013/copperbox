{-# LANGUAGE MultiParamTypeClasses      #-}
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
-- Matrix type
--
--------------------------------------------------------------------------------


module Wumpus.Core.Matrix (
  -- * Matrix types
  Matrix2'2(..), DMatrix2'2,
  Matrix3'3(..), DMatrix3'3,

  -- * Construct identity matrix
  IdentityMatrix(..),

  -- * Transposition
  Transpose(..),

  -- * Determinant
  Determinant(..),
  invertible,

  -- * Square Matrices
  SquareMatrix(..),

  -- * Common transformation matrices (for 2D homogeneous coordinates)
  scalingMatrix,
  translationMatrix,
  rotationMatrix,
  rotationMatrix'
 
  ) where



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
-- Common transformation matrices (for 2d homogeneous coordinates)

scalingMatrix :: Num a => a -> a -> Matrix3'3 a
scalingMatrix sx sy = M3'3  sx 0 0   0 sy 0   0 0 1

translationMatrix :: Num a => a -> a -> Matrix3'3 a
translationMatrix x y = M3'3 1 0 x  0 1 y  0 0 1




rotationMatrix :: Floating a => a -> Matrix3'3 a
rotationMatrix ang = M3'3 (cos ang) (- sin ang) 0 
                          (sin ang) (cos ang)   0  
                          0         0           1

rotationMatrix' :: Floating a => a -> a -> a -> Matrix3'3 a
rotationMatrix' ang x y = mT * (rotationMatrix ang) * mTinv
  where
    mT    = M3'3 1 0 x     0 1 y     0 0 1
    mTinv = M3'3 1 0 (-x)  0 1 (-y)  0 0 1

 
-- No reflectionMatrix function
-- A reflection about the x-axis is a scale of 1 (-1)
-- A reflection about the y-axis is a scale of (-1) 1
