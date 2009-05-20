{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wumpus.Matrix
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Datatype and primitives to handle the CTM (current-transformation-matrix)
--
--------------------------------------------------------------------------------


module Graphics.Wumpus.Matrix where

import Graphics.Wumpus.Vector

data Matrix2'2 a = M2'2 !a !a !a !a
  deriving (Eq)

type DMatrix2'2 = Matrix2'2 Double

instance Show a => Show (Matrix2'2 a) where
  show (M2'2 a b c d) = "(M2'2 " ++ show [[a,b],[c,d]] ++ ")"  


instance Num a => Num (Matrix2'2 a) where
  (+) (M2'2 a b c d) (M2'2 e f g h) = M2'2 (a+e) (b+f) (c+g) (d+h)
  (-) (M2'2 a b c d) (M2'2 e f g h) = M2'2 (a-e) (b-f) (c-g) (d-h)
  (*) (M2'2 a b c d) (M2'2 e f g h) = M2'2 (a*e+b*g) (a*f+b*h) (c*e+d*g) (c*f+d*h)
  
  abs (M2'2 a b c d)      = M2'2 (abs a) (abs b) (abs c) (abs d)
  negate (M2'2 a b c d)   = M2'2 (negate a) (negate b) (negate c) (negate d)
  signum (M2'2 a b c d)   = M2'2 (signum a) (signum b) (signum c) (signum d)
  fromInteger a           = M2'2 (fromInteger a) (fromInteger a) 
                                 (fromInteger a) (fromInteger a)
  


class IdentityMatrix t where
  identityMatrix :: Num a => t a 

instance IdentityMatrix Matrix2'2 where
  identityMatrix = M2'2 1 0 0 1
  

-- type Vector2  = Pair Double
-- type Matrix22 = Matrix22' Double

data PsMatrix = PsMatrix { matrix_component :: DMatrix2'2, vector_component :: DVec2 }
  deriving (Eq,Show)


-- square22 :: Double -> Double -> Double -> Double -> SquareMatrix Pair Double
-- square22 a b c d = SquareMatrix (Pair (Pair(a,b), Pair(c,d)))

-- vect2 :: Double -> Double -> Pair Double
-- vect2 e f = Pair (e,f)


initmatrix :: PsMatrix
initmatrix = PsMatrix identityMatrix vzero


multiply :: PsMatrix -> PsMatrix -> PsMatrix
multiply (PsMatrix m v) (PsMatrix m' v') = PsMatrix (m*m') (v*v')

translate :: Double -> Double -> PsMatrix -> PsMatrix
translate trx try (PsMatrix m v) = 
    PsMatrix (m * identityMatrix) (v * V2 trx try)

rotate :: Double -> PsMatrix -> PsMatrix
rotate ang (PsMatrix m v) = 
    PsMatrix (m * M2'2 (cos ang) (sin ang) (negate $ sin ang) (cos ang))
             (v * V2 0 0)                                 


scale :: Double -> Double -> PsMatrix -> PsMatrix
scale sx sy (PsMatrix m v) = PsMatrix (scaleSq sx sy m) v


scaleSq :: Double -> Double -> DMatrix2'2 -> DMatrix2'2
scaleSq sx sy m =  (M2'2 sx 0 0 sy) * m


printmatrix :: PsMatrix -> String
printmatrix (PsMatrix m v) = ('[' :) . (fm m `sp` fv v) . (']':) $ []
  where
    fm (M2'2 a b  c d)  = shows a `sp`  shows b `sp` shows c `sp` shows d
    fv (V2 e f)         = shows e `sp` shows f

    sp :: ShowS -> ShowS -> ShowS
    sp l r = l . showChar ' ' . r






