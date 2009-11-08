{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.Base
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Type classes and common things.
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.Base where

import Data.List ( mapAccumR )


-- | Reverse the direction of some ordered object (line segment, 
-- bezier curve, ...).
class Converse a where 
  converse :: a -> a



-- Subdivide a positive non-zero fractional number n times 
-- returning the list @[0, a/n, 2*a/n, ..., a]@  
subdivisions :: Fractional a => Int -> a -> [a]
subdivisions i a = take (i+1) $ iterate (+n) 0 where 
   n = a / fromIntegral i


-- | paramorphism (generalizes cata (foldr), folds right...)
para :: (a -> ([a], b) -> b) -> b -> [a] -> b
para phi b = step
  where step []     = b
        step (x:xs) = phi x (xs, step xs)


-- | Rotate the list through a cirle...
-- This function is used, but is it useful? 
-- Less gnomically - maybe it should be a generator (@ t -> [t] @) 
-- from some initial value (i.e. a point) rather than a 
-- transformer (@ [t] -> [t] @).
circular :: (Floating a, Real a, MatrixMult Matrix3'3 t, MatrixParam t ~ a, Rotate t) 
         => [t] -> [t]
circular xs = snd $ mapAccumR fn 0 xs 
  where
    fn ang a = (ang+1, rotate (2*ang*pi/len) a)
    len      = fromIntegral $ length xs

