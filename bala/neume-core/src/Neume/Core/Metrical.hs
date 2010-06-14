{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Metrical
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Beaming and metrical types and utilities.
--
-- Extremities - printable beam groups should only begin and end 
-- on (notes, chords) and not rests. 
--
--------------------------------------------------------------------------------

module Neume.Core.Metrical
  ( 

    BeamExtremity(..)
  , DMeasure(..)

  -- * Plet multiplier
  , PletMult
  , MultiplierStack
  , mult_stack_zero
  , pushPM
  , scaleFactor
  , nmeasureCtx

  -- * Meter patterns
  , MeterPattern
  , makeMeterPattern
  , compoundMeter
  , simpleMeter


  , sminus
  , splus

  -- * Anacrusis
  , anacrusis

  ) where

import Neume.Core.Duration
import Neume.Core.Utils.Common ( makeRational, modR )

import Data.Ratio




--------------------------------------------------------------------------------



class BeamExtremity a where 
   rendersToNote :: a -> Bool
   

class DMeasure a where
  dmeasure :: a -> DurationMeasure

instance DMeasure Duration where
  dmeasure = extent


-- Store plet-multipliers as pairs of integers rather than a 
-- Rational as a Rational normalizes the fraction.
-- 
-- > PletMult = (numerator, denominator)
--
type PletMult = (Integer,Integer)

type MultiplierStack = [Rational]

mult_stack_zero :: MultiplierStack
mult_stack_zero = []


-- TODO - ensure that plet multiplier is used correctly with ABC
-- which may want the numerator / denominator in the other order
--
pushPM :: PletMult -> MultiplierStack -> MultiplierStack
pushPM (p,q) stk = (p%q) : stk

scaleFactor :: MultiplierStack -> DurationMeasure
scaleFactor []     = (1%1)
scaleFactor (x:xs) = x * scaleFactor xs


nmeasureCtx :: DMeasure a => MultiplierStack -> a -> DurationMeasure
nmeasureCtx stk a = dmeasure a * (scaleFactor stk)



--------------------------------------------------------------------------------
-- Meter patterns


-- Implementation note - MeterPatterns must support arithmetic
-- so are lists of Rationals rather that the lists of Duration.


type MeterPattern = [Rational] 
     

makeMeterPattern :: Int -> Int -> MeterPattern
makeMeterPattern n d 
      | compoundMeter  n d  = replicate 3 $ (makeRational n d) / 3
      | simpleMeter n d     = replicate n $ makeRational 1 d
      | otherwise           = error $ err_msg
  where
    err_msg = "meterPattern - can't generate a meter pattern for a "
           ++ "meter that is neither simple or compound."

-- Note compoundMeter and simpleMeter overlap

compoundMeter :: Integral a => a -> a -> Bool
compoundMeter n d = log2whole d && (n `mod` 3 == 0)
         
simpleMeter :: Integral a => a -> a -> Bool
simpleMeter _ d = log2whole d

log2whole :: Integral a => a -> Bool
log2whole = (==0) . snd . pf . logBase 2 . fromIntegral where
    pf :: Double -> (Int, Double)
    pf = properFraction



--------------------------------------------------------------------------------
-- Stack (meter pattern) addition and subtraction

-- | sminus subtracts from the top of stack.
--
-- If the number to be subtracted is greater than the top of the
-- stack the top of the stack is popped and the remainder is 
-- subtracted from the new stack top (the stack will never 
-- contain negative numbers).
-- 
--

sminus :: (Num a, Ord a) => a -> [a] -> [a]
sminus a xs     | a < 0 = splus (abs a) xs
sminus _ []             = []
sminus a (x:xs)         = step (x-a) xs where
  step r ys      | r > 0     = r:ys
  step r (y:ys)  | r < 0     = step (y - abs r) ys
  step _ ys                  = ys                   -- empty stack or r==0
  

-- | splus always conses the scalar (unless it is negative, which 
-- is treated as stack-minus).
--    
splus :: (Num a, Ord a) => a -> [a] -> [a]
splus a xs | a > 0     = a:xs
           | a < 0     = sminus (abs a) xs
           | otherwise = xs

--------------------------------------------------------------------------------
-- Anacrusis



-- | Shorten a meter pattern to represent an anacrusis of a note 
-- or notes before the first bar. Figuratively the anacrusis 
-- /counts from the right/. 
--
-- Negative durations result in a runtime error.
--
anacrusis :: DurationMeasure -> MeterPattern -> MeterPattern
anacrusis d0 mp  | d0 == 0   = mp
                 | d0 <  0   = error "anacrusis - negative duration"
                 | otherwise = let len = sum mp in step (d0 `modR` len) len
  where
    step d len = sminus (len - d) mp 

