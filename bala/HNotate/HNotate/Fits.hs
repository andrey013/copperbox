{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
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
-- 'Fitting' and measuring.
--
--------------------------------------------------------------------------------

module HNotate.Fits (
  Fits(..),
  splitRV,
  splitSequenceHy,
  strides,
  sumMeasure, 
  sumSegments,
  segment,
  anasegment,
  regiment,
  anaregiment,
  advance,
  -- testing
  prop_regiment_same_lengths_as_sequence
 ) where

-- Should only depend on Duration as its essential to have a 
-- Fits instance for Duration.

import HNotate.Duration

import qualified Data.Foldable as F
import Data.Sequence hiding (length)
import Prelude hiding (null)



--------------------------------------------------------------------------------
-- 'Fitting' 


class (Ord b, Num b) => Fits a b | a -> b where
  measure   :: a -> b
  split     :: b -> a -> (a,a)
  hyphenate :: a -> Maybe a 
  consumes  :: a -> b -> b
  
  -- default
  consumes a b = b - measure a   
  hyphenate = const Nothing


-- By how much does an element _exceed_ the width?
-- (How much does it stride over the width?)
-- A positive answer indicates the element is bigger than the width. 
-- A negative one - that it is smaller.
strides   :: Fits a b => a -> b -> b
strides a b = negate $ b - measure a  

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
              in (updateDuration ld a, updateDuration rd a)          

-- With (- -XUndecidableInstances -)this could be relaxed to  
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

                
-- regiment - a non splitting segment function
-- Also it splits on a list of widths not a single one. 
-- This is because regiment's primary use is to split a bar 
-- according to a meter pattern.
--  
-- Use two accumulators: one for Seq (Seq a) and one for Seq a
regiment :: Fits a b => Seq a -> [b] -> Seq (Seq a)
regiment se ls = step empty empty (viewl se) ls where
    step :: Fits a b => Seq (Seq a) -> Seq a -> ViewL a -> [b] -> Seq (Seq a)
    step acc ac EmptyL    _      = if null ac then acc else acc |> ac
    step acc ac (a :< sa) []     = acc |>  ((ac |> a) >< sa)
    step acc ac (a :< sa) (x:xs)  
    -- The simple case 'a' fits in 'x' so append it to the current 
    -- accumulator and carry on...
        | measure a < x   = step acc (ac |> a) (viewl sa) 
                                      (advance (measure a) (x:xs))
        
    -- An exact fit!
        | measure a == x  = step (acc |> (ac |> a)) empty (viewl sa) xs
         
    -- More complicated 'a' doesn't fit in 'x', see if it fits in the 
    -- next regiment or if it jumps (i.e. fills more than one regiment)                                          
        | otherwise       = let (jmp,ys) = jumpedAdvance (measure a) (x:xs) in
            case jmp of
    -- True - we've jumped that means 'a' was big enough to fill a 
    -- regiment by itself
              True -> step (acc |> ac |> singleton a) empty (viewl sa) ys
              
    -- False - 'a' starts a new regiment (alternatively it could join 'ac' 
    -- as as 'ac' joins 'acc', either way is acceptable).  
              False -> step (acc |> ac) (singleton a) (viewl sa) ys  
            
anaregiment :: Fits a b => b -> Seq a -> [b] -> Seq (Seq a)
anaregiment asis se xs = regiment se ys where
    -- note that an anacrusis is how much left to consume,
    -- not how much consumed.
    ys = advance (rlen - asis) xs
    rlen = F.sum xs
 




-- have you consumed more than on regiment measure?
jumpedAdvance :: (Num a, Ord a) => a -> [a] -> (Bool,[a])
jumpedAdvance a xs = let ys = advance a xs in
    if length xs > length ys + 1 then (True,ys) else (False, ys)     
        
advance :: (Num a, Ord a) => a -> [a] -> [a]
advance _ []                  = []
advance a (x:xs)  | a == x    = xs
                  | a >  x    = advance (a-x) xs
                  | otherwise = (x-a) : xs

prop_regiment_same_lengths_as_sequence :: Fits a b => Seq a -> [b] -> Bool
prop_regiment_same_lengths_as_sequence se xs = sumMeasure se == sumSegments sse
  where sse = regiment se xs
  
   