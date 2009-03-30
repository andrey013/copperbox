{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Fits
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  mptc, fundeps
--
-- 'Fitting' and segmenting.
--
--------------------------------------------------------------------------------

module HNotate.Fits where


import qualified Data.Sequence as S


class (Ord b, Num b) => Fits a b | a -> b where
  -- @measure@ object a by the unit b
  measure   :: a -> b 
  -- @fit@ compares a's measure to the size b, a positive number 
  -- gives the amount /leftover/, negative indicates the amount by which 
  -- a's measure exceeds b.
  fit       :: a -> b -> b    
  fits      :: a -> b -> Bool
 
  fit a b = b - measure a
  
  fits a = (>=0) . fit a


data Fit a = AllLeft a | Split a a | AllRight a

class Fits a b => Partition a b | a -> b where
  partition :: b -> a -> Fit a 


{-

fitfit :: Fits a b => a -> b -> Fit a
fitfit a b      | b <= 0      = AllRight a
fitfit a b      | fits a b    = AllLeft  a
fitfit a _                    = Split a

-}

   
