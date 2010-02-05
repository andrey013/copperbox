{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  M2.Segment
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Segmenting recursion
--
--------------------------------------------------------------------------------

module M2.Segment
  ( 
  
    NumMeasured(..)

  , segment
  , segStep

  , cb, CB(..)

  ) where




-- This is the Measured class from the FingerTree paper and 
-- library but with Num for the superclass rather than Monoid

class Num (Measurement a) => NumMeasured a where
  type Measurement a
  nmeasure :: a -> Measurement a

instance NumMeasured Char where
  type Measurement Char = Int
  nmeasure 'a' = 0
  nmeasure _   = 1





data CB a = Carry a | Borrow a
  deriving (Eq,Show)



segment :: (Measurement a ~ v, NumMeasured a, Ord v, Num v) 
        => [a] -> [v] -> [[a]]
segment [] _      = []
segment xs []     = [xs]
segment xs (y:ys) = (bs:bss) where
                    (bs, (diff,xs')) = segStep xs y
                    bss              = segment xs' (cb diff ys)


-- carry-borrow interpretation is crucial...

cb :: (Num a, Ord a) => CB a -> [a] -> [a] 
cb (Carry a)  xs      | a >  0    = a:xs
                      | a == 0    = xs
cb (Borrow a) (x:xs)  | a >  x    = cb (Borrow $ a-x) xs
                      | a == x    = xs
                      | otherwise = (x-a):xs 
cb _          _                   = []
                         



segStep :: (Measurement a ~ v, NumMeasured a, Ord v, Num v) 
        => [a] -> v -> ([a], (CB v, [a]))
segStep []     _  = ([], (Carry 0, []))
segStep (x:xs) v  = step $ nmeasure x where
  step xd | xd >  v   = ([x], (Borrow $ negate $ v-xd, xs))
          | xd == v   = ([x], (Carry 0, xs))
          | otherwise = (x:xs', ans) where (xs',ans) = segStep xs (v-xd)







