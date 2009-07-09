{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Transformations
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


module Wumpus.Core.Transformations 
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

rotate :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
       => Radian -> t a -> t a 
rotate a = ((rotationMatrix a) *#) 

rotateAbout :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
            => Radian -> Point2 a -> t a -> t a 
rotateAbout a (P2 x y) = ((rotationMatrix' a x y) *#) 

rotate90 :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
         => t a -> t a 
rotate90 = rotate (pi/2) 

rotate90About :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
              => Point2 a -> t a -> t a 
rotate90About = rotateAbout (pi/2)


rotate30 :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
         => t a -> t a 
rotate30 = rotate (pi/6) 

rotate30About :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
              => Point2 a -> t a -> t a 
rotate30About = rotateAbout (pi/6)


rotate45 :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
         => t a -> t a 
rotate45 = rotate (pi/4) 

rotate45About :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
              => Point2 a -> t a -> t a 
rotate45About = rotateAbout (pi/4)


rotate60 :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
         => t a -> t a 
rotate60 = rotate (2*pi/3) 

rotate60About :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
              => Point2 a -> t a -> t a 
rotate60About = rotateAbout (2*pi/3)


rotate120 :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
          => t a -> t a 
rotate120 = rotate (4*pi/3) 

rotate120About :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
               => Point2 a -> t a -> t a 
rotate120About = rotateAbout (4*pi/3)


circular :: (Floating a, Real a, MatrixMult Matrix3'3 t) 
         => [t a] -> [t a]
circular xs = snd $ mapAccumR fn 0 xs 
  where
    fn ang a = (ang+1, rotate (2*ang*pi/len) a)
    len      = fromIntegral $ length xs


scale :: (Num a, MatrixMult Matrix3'3 t) => a -> a -> t a -> t a 
scale x y = ((scalingMatrix x y) *#) 

uniformScale :: (Floating a, MatrixMult Matrix3'3 t) => a -> t a -> t a 
uniformScale a = scale a a 


-- | translate @x@ @y@.
translate :: (Floating a, MatrixMult Matrix3'3 t) => a -> a -> t a -> t a 
translate x y = ((translationMatrix x y) *#)

translateBy :: (Floating a, MatrixMult Matrix3'3 t) => Vec2 a -> t a -> t a 
translateBy (V2 x y) = translate x y

reflectX :: (Num a, MatrixMult Matrix3'3 t) => t a -> t a
reflectX = scale (-1) 1

reflectY :: (Num a, MatrixMult Matrix3'3 t) => t a -> t a
reflectY = scale 1 (-1)


