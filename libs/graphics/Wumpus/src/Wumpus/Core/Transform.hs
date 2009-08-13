{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Transform
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Transformations
--
--------------------------------------------------------------------------------


module Wumpus.Core.Transform
  (
  -- * Rotation
    rotate
  , rotateAbout
  , rotate90
  , rotate90About
  , rotate30
  , rotate30About
  , rotate45
  , rotate45About
  , rotate60
  , rotate60About
  , rotate120
  , rotate120About

  -- * Iterated rotation
  , circular

  -- * Scale
  , scale
  , uniformScale

  -- * Translation
  , translate
  , translateBy

  -- * Reflection
  , reflectX
  , reflectY

  ) where


import Wumpus.Core.Matrix
import Wumpus.Core.Point 
import Wumpus.Core.Radian
import Wumpus.Core.Vector 


import Data.List ( mapAccumR )

rotate :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
       => Radian -> t -> t
rotate a = ((rotationMatrix a) *#) 

rotateAbout :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a) 
            => Radian -> Point2 a -> t -> t 
rotateAbout a (P2 x y) = ((rotationMatrix' a x y) *#) 

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


