{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Mergesort
-- Copyright   :  (c) ??
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Mergesort for sequences.
-- Acknowledgement - Original by Ian Lynagh, 
-- see Data.List in the Hierarchical Libraries.
-- 
-- Minimally adapted for Data.Sequence.
--
--------------------------------------------------------------------------------

module ZMidi.Mergesort (
  mergesort
  ) where

import Data.Sequence
import Prelude hiding (length)


-- Acknowledgement - this is Ian Lynagh's mergesort from the Data.List
-- source translated to work on Seq.
mergesort :: (a -> a -> Ordering) -> Seq a -> Seq a
mergesort cmp = mergesort' cmp . fmap singleton

mergesort' :: (a -> a -> Ordering) -> Seq (Seq a) -> Seq a
mergesort' cmp se | length se >=2 = mergesort' cmp (merge_pairs cmp se)
                  | otherwise     = step (viewl se)
  where
    step EmptyL       = empty
    step (sa :< _)    = sa        -- rhs is known to be empty!                 


                     

merge_pairs :: (a -> a -> Ordering) -> Seq (Seq a) -> Seq (Seq a)
merge_pairs cmp se = step1 (viewl se) where
    step1 EmptyL          =  empty
    step1 (sa :< ssa)     =  step2 sa (viewl ssa)
    
    step2 _  EmptyL       = se
    step2 sa (sb :< ssb)  = merge cmp sa sb <| merge_pairs cmp ssb
        


merge :: (a -> a -> Ordering) -> Seq a -> Seq a -> Seq a
merge cmp se se' = step (viewl se) (viewl se') where
    step _          EmptyL        = se
    step EmptyL     _             = se'
    step (x :< sx)  (y :< sy)     = case x `cmp` y of
                                      GT -> y <| merge cmp (x <| sx) sy
                                      _  -> x <| merge cmp sx        (y <| sy)