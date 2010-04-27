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
    Metric
  , diffMetric

  ) where


data Metric n = Metric 
      { metric_name         :: String
      , metric_description  :: String 
      , metric_value        :: n
      }
  deriving (Eq,Show)

diffMetric :: (n -> n -> b) -> Metric n -> Metric n -> b
diffMetric cmp (Metric _ _ a) (Metric _ _ b) = cmp a b