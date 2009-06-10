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

rotate :: (Floating a, VecMult Matrix3'3 t) => a -> t a -> t a 
rotate a = ((rotationMatrix a) *#) 

rotate90 :: (Floating a, VecMult Matrix3'3 t) => t a -> t a 
rotate90 = rotate (pi/2) 

rotate45 :: (Floating a, VecMult Matrix3'3 t) => t a -> t a 
rotate45 = rotate (pi/4) 

rotate60 :: (Floating a, VecMult Matrix3'3 t) => t a -> t a 
rotate60 = rotate (2*pi/3) 


scale :: (Floating a, VecMult Matrix3'3 t) => a -> t a -> t a 
scale a = ((scalingMatrix a a) *#) 

translate :: (Floating a, VecMult Matrix3'3 t) => a -> a -> t a -> t a 
translate x y = ((translationMatrix x y) *#)
