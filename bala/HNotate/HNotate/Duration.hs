{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Duration
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Duration representation.
--
--------------------------------------------------------------------------------

module HNotate.Duration (
    -- Data type
    Duration, 
    
    RhythmicValue(..),
    
    makeDuration,
    
    -- particular durations
    duration_zero, no_duration,
    
    dotn,
    
    
    -- * Helper for ratios
    ratioElements, convRational, convRatio,
    durationToDouble, divModR,

    
    -- * printing
    PrintableDuration(..), printableDuration,
    pdElements,

    approximateDuration,
    

    -- * Named instances (American)
    longa, 
    -- $amerdoc
    double_whole, whole, half, quarter, eighth, sixteenth, thirty_second,
    sixty_fourth, one_hundred_twenty_eighth,
    
    -- * Named instances (English)
    -- $engdoc
    breve, semibreve, minim, crochet, quaver, semiquaver, demisemiquaver,
    hemidemisemiquaver, semihemidemisemiquaver,
    
    -- * Named instances (Shorthand)
    du1, du2, du4, du8, du16, du32, du64, du128    

  ) where

-- Avoid internal dependencies as this module is included in Bala

import Data.List (unfoldr)
import Data.Ratio


type Duration = Rational 

class RhythmicValue a where
  rhythmicValue   :: a -> Duration
  modifyDuration  :: a -> Duration -> a

instance RhythmicValue Duration where
  rhythmicValue     = id
  modifyDuration    = const
    
  

makeDuration :: Integral a => a -> a -> Duration
makeDuration n d = fromIntegral n % fromIntegral d

duration_zero :: Duration
duration_zero = 0

no_duration :: Duration
no_duration = 0

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
      step k | k < r      = k
             | otherwise  = step (k / 2)
    
    
    dotting :: Rational -> Rational -> Int
    dotting d dapprox = step 1 where
      step i | dotn i dapprox > d = (i-1)
             | i > 3              = i -- to do (dotting doesn't converge...)
             | otherwise          = step (i+1) 
                              

      
--------------------------------------------------------------------------------
-- Named elements




longa                       :: Duration
longa                       = 4%1

-- $amerdoc
-- American naming.
double_whole                :: Duration
double_whole                = 2%1

whole                       :: Duration
whole                       = 1%1

half                        :: Duration
half                        = 1%2

quarter                     :: Duration
quarter                     = 1%4

eighth                      :: Duration
eighth                      = 1%8

sixteenth                   :: Duration
sixteenth                   = 1%16

thirty_second               :: Duration
thirty_second               = 1%32

sixty_fourth                :: Duration
sixty_fourth                = 1%64

one_hundred_twenty_eighth   :: Duration
one_hundred_twenty_eighth   = 1%128

-- $engdoc
-- English naming.
breve                       :: Duration
breve                       = double_whole

semibreve                   :: Duration
semibreve                   = whole

minim                       :: Duration
minim                       = half

crochet                     :: Duration
crochet                     = quarter

quaver                      :: Duration 
quaver                      = eighth

semiquaver                  :: Duration 
semiquaver                  = sixteenth

demisemiquaver              :: Duration
demisemiquaver              = thirty_second

hemidemisemiquaver          :: Duration 
hemidemisemiquaver          = sixty_fourth

semihemidemisemiquaver      :: Duration
semihemidemisemiquaver      = one_hundred_twenty_eighth

-- Shorthands
du1   :: Duration
du1   = whole

du2   :: Duration
du2   = half

du4   :: Duration
du4   = quarter

du8   :: Duration
du8   = eighth

du16  :: Duration
du16  = sixteenth

du32  :: Duration
du32  = thirty_second

du64  :: Duration
du64  = sixty_fourth

du128 :: Duration
du128 = one_hundred_twenty_eighth
