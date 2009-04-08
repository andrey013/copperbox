{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Duration
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Duration representation.
--
--------------------------------------------------------------------------------

module Mullein.Duration (
    -- Data type
    Duration, 
    
    Temporal(..),
    Spacer(..),
    
    makeDuration,
    
    -- particular durations
    duration_zero, no_duration,
    
    dot, dotn,
    
    
    -- * Helper for ratios
    ratioElements, convRational, convRatio,
    durationToDouble, divModR,


    -- * printing
    PrintableDuration(..), printableDuration,
    ppDuration, 
    
    pdElements,

    approximateDuration,
    

  ) where


import Data.List (unfoldr)
import Data.Ratio

import Text.PrettyPrint.Leijen hiding ( dot )

type Duration = Rational 

class Temporal a where
  duration      :: a -> Duration
  swapDuration  :: Duration -> a -> a

instance Temporal Duration where
  duration      = id
  swapDuration  = const

class Spacer a where
  spacer :: Duration -> a
   

makeDuration :: Integral a => a -> a -> Duration
makeDuration n d = fromIntegral n % fromIntegral d

duration_zero :: Duration
duration_zero = 0

no_duration :: Duration
no_duration = 0

dot :: Duration -> Duration
dot = dotn 1

dotn :: Int -> Duration -> Duration
dotn i d | i < 1 = d
         | otherwise  = d + step i (d /2)
  where
    step 0 _ = 0
    step n d' = d' + step (n-1) (d' / 2)
             
    
ratioElements :: Integral a => Ratio a -> (a,a)
ratioElements r = (numerator r, denominator r)

rfork :: Integral a => (a -> b) -> Ratio a -> (b,b)
rfork f r = (f $ numerator r, f $ denominator r)

convRational :: Integral a => Rational -> Ratio a
convRational = uncurry (%) . rfork fromIntegral

convRatio :: Integral a => Ratio a -> Rational
convRatio = uncurry (%) . rfork fromIntegral

durationToDouble :: Duration -> Double
durationToDouble = uncurry (/) . rfork fromIntegral



  

--------------------------------------------------------------------------------
-- divMod (with rounding) for rationals 

-- check - 8.0 `divModR` 0.75

-- prop_mod_postive a b = let (_,md) = a `divModR` b in signum md == 1

divModR :: (Integral b) => Ratio b -> Ratio b -> (b, Ratio b)
divModR a b = let a1 = a / b; a2 = floor a1 in (a2, a-((a2%1)*b))


   
data PrintableDuration = PrintableDuration { 
    _duration  :: Rational,
    _dot_count :: Int 
  }
  deriving (Eq,Ord,Show)


-- | @'rationalize'@ - turn a PrintableDuration back
-- into a Rational which may normalize it. 
rationalize :: PrintableDuration -> Rational
rationalize (PrintableDuration r n) | n >  0     = sum $ r : unfoldr phi (n,r/2)
                                    | otherwise  = r 
  where 
    phi (0,_) = Nothing
    phi (i,a) = Just (a,(i-1,a/2)) 
    
pdElements :: PrintableDuration -> (Int,Int,Int)  
pdElements (PrintableDuration r dc) = 
  let (n,d) = ratioElements r in (fromIntegral n, fromIntegral d, dc) 


printableDuration :: Duration -> PrintableDuration
printableDuration r  
    | r <= 0    = PrintableDuration duration_zero 0
    | otherwise = if r == rationalize r' then r' else PrintableDuration r 0
  where 
    r' = approximateDuration r 
    


    
approximateDuration :: Duration -> PrintableDuration
approximateDuration drn 
    | drn > 1   = let approxd = nearestUp drn
                  in PrintableDuration approxd (dotting drn approxd)
    | drn < 1   = let approxd = nearestDown drn
                  in PrintableDuration approxd (dotting drn approxd)
    | otherwise = PrintableDuration (1%1) 0
  where
    nearestUp :: Rational -> Rational
    nearestUp r = step (1 % 1) (2 * (1 % 1)) where
      step n k | k > r      = n
               | otherwise  = step k (2 * k)
       
    
    nearestDown :: Rational -> Rational
    nearestDown r = step ((1 % 1) / 2) where
      step k | k <= r     = k
             | otherwise  = step (k / 2)
    
    
    dotting :: Rational -> Rational -> Int
    dotting d dapprox | d == dapprox  = 0
                      | otherwise     = step 1 where
      step i | dotn i dapprox > d = (i-1)
             | i > 3              = i
             | otherwise          = step (i+1) 

ppDuration :: Duration -> Doc 
ppDuration = pretty . printableDuration

instance Pretty PrintableDuration where
  pretty (PrintableDuration r dc) = let (n,d) = ratioElements r in 
      pretty n <> char '/' <> pretty d <> text (replicate dc '.') 
      
      
      
