{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Charcoal.Extra
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- ...
--
--------------------------------------------------------------------------------


module Graphics.Charcoal.Extra where 

import Graphics.Charcoal.Picture

class Norm2 a where
  norm2 :: a -> Double

instance Norm2 Vector where
  norm2 (V2 x y) = x*x+y*y

step :: (Floating a,Ord a) => a -> a
step x | x<0.0001  = 0
       | x>0.0001  = 1
       | otherwise = 0.5


unitdisk :: Vector -> Double 
unitdisk p = step (1 - norm2 p)
