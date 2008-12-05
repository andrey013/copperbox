{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.CommonUtils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  mptc, fundeps
--
-- 'Fitting' and measuring.
--
--------------------------------------------------------------------------------

module HNotate.Fits (
  Fits(..),
  splitRV,
  splitSequenceHy,
  sumMeasure, 
  sumSegments,
  segment,
  anasegment
 ) where

-- Should only depend on Duration as its essential to have a 
-- Fits instance for Duration.

import HNotate.Duration

import qualified Data.Foldable as F
import Data.Sequence hiding (reverse)
import Prelude hiding (null, length)



--------------------------------------------------------------------------------
-- 'Fitting' 


class (Ord b, Num b) => Fits a b | a -> b where
  measure   :: a -> b
  split     :: b -> a -> (a,a)
  hyphenate :: a -> Maybe a 
  
  -- default
  hyphenate = const Nothing
  

-- Pretend numeric instances are naturals
instance Fits Int Int where 
  measure = id
  split a b | a < b     = (a,0)
            | otherwise = (b, a-b) 

instance Fits Duration Duration where 
  measure = id
  split a b | a < b     = (a,0)
            | otherwise = (b, a-b) 
            
splitRV :: RhythmicValue a => Duration -> a -> (a,a)
splitRV l a = let (ld,rd) = split l (rhythmicValue a) 
              in (modifyDuration a ld, modifyDuration a rd)          

-- With (- -XUndecidableInstances -)this can be relaxed to  
-- instance Fits a b => Fits (Seq a) b             
instance Fits a Duration => Fits (Seq a) Duration where
  measure   = sumMeasure
  split     = splitSequenceHy False

splitSequenceHy :: Fits a b => Bool -> b -> Seq a -> (Seq a, Seq a)
splitSequenceHy use_hy l se = step empty (viewl se) l 
  where
    step :: Fits a b => Seq a -> ViewL a -> b -> (Seq a, Seq a)
    step sa EmptyL    _   = (sa, empty)
    step sa (e :< sz) d 
        | d == 0          = (sa, e <| sz)
        | otherwise       = let ed = measure e in 
            if ed <= d then step (sa |> e) (viewl sz) (d-ed)
                       -- e must divide as d/=0 
                       else let (el,er) = split d e in (sa |-> el, er <| sz)
    
    (|->) :: Fits a b => Seq a -> a -> Seq a
    (|->) sx x = if use_hy 
                    then maybe (sx |> x) (\hy -> sx |> x |> hy) (hyphenate x)
                    else sx |> x                        
            


sumMeasure :: (Fits a b, F.Foldable c) => c a -> b
sumMeasure = F.foldr (\e a -> measure e + a) 0 


sumSegments :: Fits a b => Seq (Seq a) -> b
sumSegments = F.foldr (\a n -> sumMeasure a + n) 0


segment :: Fits a b => Bool -> b -> Seq a -> Seq (Seq a)
segment hyph d seg | d <= 0     = singleton seg
                   | otherwise  = step seg 
  where
    step s = let (l,r) = splitSequenceHy hyph d s in case null r of
                True -> singleton l
                False -> l <| step r

anasegment :: Fits a b => Bool -> b -> b -> Seq a -> Seq (Seq a)           
anasegment hyph a d seg 
    | a <= 0    = segment hyph d seg
    | otherwise = let (left,right) = splitSequenceHy hyph a seg in
                  case null right of 
                      True -> singleton left
                      False -> left <| segment hyph d right 

                

 