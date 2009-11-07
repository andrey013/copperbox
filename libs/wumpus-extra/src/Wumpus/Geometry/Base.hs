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
