
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Contour
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Intervals (and contours) on foldable containers
-- 
--------------------------------------------------------------------------------

module Bala.Base.Contour where

import qualified Data.Foldable as F

data RefinedContour = R         -- equal
                    | US        -- upwards small
                    | UL        -- upwards large                    
                    | DS        -- downwards small
                    | DL        -- downwards large
  deriving (Eq,Ord,Show)
  
interval :: (F.Foldable c, Num a) => c a -> [a] 
interval = genInterval (-)

contour :: (F.Foldable c, Num a) => c a -> [a] 
contour = genInterval (\a b -> signum $ a - b) 

ocontour :: (F.Foldable c, Ord a) => c a -> [Ordering]  
ocontour = genInterval compare

refinedContour :: (F.Foldable c, Ord a, Num a) => a -> c a -> [RefinedContour]
refinedContour limit = genInterval f where
  f a b | a > b     = if (a - b <= limit) then US else UL 
        | a < b     = if (b - a <= limit) then DS else DL 
        | otherwise = R
        
        
genInterval :: F.Foldable c => (a -> a -> b) -> c a -> [b]  
genInterval f a = let a' = F.toList a in step a' (rot1 a') where
  step (x:xs) (y:ys)  = f y x : step xs ys
  step _      _       = []
  
  rot1 []     = []
  rot1 (x:xs) = xs ++ [x]

