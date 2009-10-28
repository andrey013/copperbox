{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Utility functions
--
--------------------------------------------------------------------------------


module Wumpus.Core.Utils
  ( 


  -- * Three values  
    max3
  , min3
  , med3

  -- * truncate / print a double
  , dtrunc

  ) where

-- | max of 3
max3 :: Ord a => a -> a -> a -> a
max3 a b c = max (max a b) c

-- | min of 3
min3 :: Ord a => a -> a -> a -> a
min3 a b c = min (min a b) c


-- | median of 3
med3 :: Ord a => a -> a -> a -> a
med3 a b c = if c <= x then x else if c > y then y else c
  where 
    (x,y)                 = order a b
    order p q | p <= q    = (p,q)
              | otherwise = (q,p)



dtrunc :: Double -> String
dtrunc d | abs d < 0.0001  = "0.0"
         | d < 0.0           = '-' :  show (abs tx)
         | otherwise         = show tx
  where
    tx :: Double
    tx = (realToFrac (roundi (d*1000000.0))) / 1000000.0
 
    roundi :: RealFrac a => a -> Integer
    roundi = round

