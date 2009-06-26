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


module Wumpus.Core.Transformations where


import Wumpus.Core.Instances
import Wumpus.Core.Matrix
import Wumpus.Core.Point 
import Wumpus.Core.Vector 

import Data.List ( mapAccumR )

rotate :: (Floating a, VecMult Matrix3'3 t) => a -> t a -> t a 
rotate a = ((rotationMatrix a) *#) 

rotateAbout :: (Floating a, VecMult Matrix3'3 t) => a -> Point2 a -> t a -> t a 
rotateAbout a (P2 x y) = ((rotationMatrix' a x y) *#) 

rotate90 :: (Floating a, VecMult Matrix3'3 t) => t a -> t a 
rotate90 = rotate (pi/2) 

rotate90About :: (Floating a, VecMult Matrix3'3 t) => Point2 a -> t a -> t a 
rotate90About = rotateAbout (pi/2)


rotate30 :: (Floating a, VecMult Matrix3'3 t) => t a -> t a 
rotate30 = rotate (pi/6) 

rotate30About :: (Floating a, VecMult Matrix3'3 t) => Point2 a -> t a -> t a 
rotate30About = rotateAbout (pi/6)


rotate45 :: (Floating a, VecMult Matrix3'3 t) => t a -> t a 
rotate45 = rotate (pi/4) 

rotate45About :: (Floating a, VecMult Matrix3'3 t) => Point2 a -> t a -> t a 
rotate45About = rotateAbout (pi/4)


rotate60 :: (Floating a, VecMult Matrix3'3 t) => t a -> t a 
rotate60 = rotate (2*pi/3) 

rotate60About :: (Floating a, VecMult Matrix3'3 t) => Point2 a -> t a -> t a 
rotate60About = rotateAbout (2*pi/3)


rotate120 :: (Floating a, VecMult Matrix3'3 t) => t a -> t a 
rotate120 = rotate (4*pi/3) 

rotate120About :: (Floating a, VecMult Matrix3'3 t) => Point2 a -> t a -> t a 
rotate120About = rotateAbout (4*pi/3)


circular :: (Floating a, VecMult Matrix3'3 t) => [t a] -> [t a]
circular xs = snd $ mapAccumR fn 0 xs 
  where
    fn ang a = (ang+1, rotate (2*ang*pi/len) a)
    len      = fromIntegral $ length xs


scale :: (Num a, VecMult Matrix3'3 t) => a -> a -> t a -> t a 
scale x y = ((scalingMatrix x y) *#) 

uniformScale :: (Floating a, VecMult Matrix3'3 t) => a -> t a -> t a 
uniformScale a = scale a a 



translate :: (Floating a, VecMult Matrix3'3 t) => a -> a -> t a -> t a 
translate x y = ((translationMatrix x y) *#)

translateBy :: (Floating a, VecMult Matrix3'3 t) => Vec2 a -> t a -> t a 
translateBy (V2 x y) = translate x y

reflectX :: (Num a, VecMult Matrix3'3 t) => t a -> t a
reflectX = scale (-1) 1

reflectY :: (Num a, VecMult Matrix3'3 t) => t a -> t a
reflectY = scale 1 (-1)


