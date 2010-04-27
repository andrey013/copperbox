{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Metrics
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
--
--------------------------------------------------------------------------------


module Precis.Metrics
  (
    Metric(..)
  , diffMetric
  
  , Edit(..)
  , difference

  ) where


import Precis.Utils

import Data.List ( find )


data Metric n = Metric 
      { metric_name         :: String
      , metric_description  :: String 
      , metric_value        :: n
      }
  deriving (Eq,Ord,Show)


data Edit a = Added a | Conflict a a | Same a | Removed a
  deriving (Eq,Show)



difference :: (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [a] -> [Edit a]
difference matches conflict as bs = toListH $ checkShort bs (checkLong as id)
  where
    checkLong []     f = f
    checkLong (x:xs) f = case find (matches x) bs of
        Just b | conflict x b -> checkLong xs (f `snocH` Conflict x b) 
               | otherwise    -> checkLong xs (f `snocH` Same x)
        Nothing               -> checkLong xs (f `snocH` Added x)

    checkShort []     f = f
    checkShort (y:ys) f = case find (matches y) as of
        Just _                -> checkShort ys f   -- already found
        Nothing               -> checkShort ys (f `snocH` Removed y)



diffMetric :: (n -> n -> b) -> Metric n -> Metric n -> b
diffMetric cmp (Metric _ _ a) (Metric _ _ b) = cmp a b
