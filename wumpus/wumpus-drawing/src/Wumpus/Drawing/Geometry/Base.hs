{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Geometry.Base
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

module Wumpus.Drawing.Geometry.Base
  ( 

  -- * 2x2 Matrix
    Matrix2'2(..)
  , DMatrix2'2
  , identity2'2
  , det2'2
  , transpose2'2
  
  -- * Line in equational form 
  , LineEquation(..)
  , lineEquation

  -- * Functions
  , affineComb
  , midpoint
  , lineAngle
  ) 
  where

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace




-- | 2x2 matrix, considered to be in row-major form.
-- 
-- > (M2'2 a b
-- >       c d)
--
-- 

data Matrix2'2 u = M2'2 !u !u   !u !u
  deriving (Eq)

type DMatrix2'2 = Matrix2'2 Double


type instance DUnit (Matrix2'2 u)   = u


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

-- | Line in equational form, i.e. @Ax + By + C = 0@.
--
data LineEquation u = LineEquation 
      { line_eqn_A :: !u
      , line_eqn_B :: !u
      , line_eqn_C :: !u 
      }
  deriving (Eq,Show)

type instance DUnit (LineEquation u) = u

lineEquation :: Num u => Point2 u -> Point2 u -> LineEquation u
lineEquation (P2 x1 y1) (P2 x2 y2) = LineEquation a b c 
  where
    a = y1 - y2
    b = x2 - x1
    c = (x1*y2) - (x2*y1)


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



