{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wumpus.Vector
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Vector type
--
--------------------------------------------------------------------------------


module Graphics.Wumpus.Vector where

data Vec3 a = V3 !a !a !a
  deriving (Eq,Show)

type DVec3 = Vec3 Double 

instance Num a => Num (Vec3 a) where
  (+) (V3 a b c) (V3 x y z) = V3 (a+x) (b+y) (c+z)
  (-) (V3 a b c) (V3 x y z) = V3 (a-x) (b-y) (c-z)
  (*) (V3 a b c) (V3 x y z) = V3 (a*x) (b*y) (c*z)
  abs (V3 a b c)            = V3 (abs a) (abs b) (abs c)
  negate (V3 a b c)         = V3 (negate a) (negate b) (negate c)
  signum (V3 a b c)         = V3 (signum a) (signum b) (signum c)
  fromInteger i             = V3 (fromInteger i) (fromInteger i) (fromInteger i)

instance Fractional a => Fractional (Vec3 a) where
  (/) (V3 a b c) (V3 x y z) = V3 (a/x) (b/y) (c/z)
  recip (V3 a b c)          = V3 (recip a) (recip b) (recip c)
  fromRational a            = V3 (fromRational a) (fromRational a) (fromRational a)


data Vec2 a = V2 !a !a
  deriving (Eq,Show)

type DVec2 = Vec2 Double

instance Num a => Num (Vec2 a) where
  (+) (V2 a b) (V2 x y) = V2 (a+x) (b+y)
  (-) (V2 a b) (V2 x y) = V2 (a-x) (b-y)
  (*) (V2 a b) (V2 x y) = V2 (a*x) (b*y)
  abs (V2 a b)          = V2 (abs a) (abs b)
  negate (V2 a b)       = V2 (negate a) (negate b)
  signum (V2 a b)       = V2 (signum a) (signum b)
  fromInteger i         = V2 (fromInteger i) (fromInteger i)

instance Fractional a => Fractional (Vec2 a) where
  (/) (V2 a b) (V2 x y) = V2 (a/x) (b/y)
  recip (V2 a b)        = V2 (recip a) (recip b)
  fromRational a        = V2 (fromRational a) (fromRational a)



infixl 8 *> 
class ScalarMult t where 
  (*>) :: Num a => a -> t a -> t a

instance ScalarMult Vec2 where
  o *> (V2 a b) = V2 (o*a) (o*b)

instance ScalarMult Vec3 where
  o *> (V3 a b c) = V3 (o*a) (o*b) (o*c)



infixl 7 .>

class ScalarProduct t where
  (.>) :: Num a => t a -> t a -> a


instance ScalarProduct Vec2 where
  (.>) (V2 a b) (V2 x y) = a*x + b*y

instance ScalarProduct Vec3 where
  (.>) (V3 a b c) (V3 x y z) = a*x + b*y + c*z


-- two vectors in R2 are perpendicular iff their dot product is 0
perp :: DVec2 -> DVec2 -> Bool
perp = ((==0) .) . (.>)

class VZero a where
  vzero :: a

instance Num a => VZero (Vec2 a) where
  vzero = V2 0 0 
 
instance Num a => VZero (Vec3 a) where
  vzero = V3 0 0 0 







