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
-- Matrix type
--
--------------------------------------------------------------------------------


module Graphics.Wumpus.Matrix where


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
  





