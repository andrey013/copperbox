{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Duration
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

module Bala.Base.Duration (
  module HNotate.Duration,

  ) where

import Bala.Base.BaseExtra

import HNotate.Duration

import Data.List
import Data.Ratio

{-

rationalDuration :: Rational -> Duration
rationalDuration r = let (n,d) = nr r in 
    if r < (2%1) then recsmall n d 0 else reclarge r (Dur (closest r) 0)
  where
    recsmall n d dots 
        | n == 0 || d == 0  = error $ "rationalDuration - doesn't simplify "
                                       ++ show r
        | n == 1            = Dur (n%d) dots
        | otherwise         = let (n',d') = nr $ (n%d) - (1%d)
                              in recsmall n' d' (dots + 1)
     
    reclarge r dur = case rationalize (dot dur) `compare` r  of
                        EQ -> dur
                        GT -> Dur r 0 -- a nonstandard duration 
                        LT -> reclarge r (dot dur)

    closest r = let ls = map (flip (%) 1) base2bases
                in last $ takeWhile (r>=) ls
                
    
-}    

{-
--    simplify n d = nr (n%d)
         
-- | Extract a \simple\ ratio plus dotting count 
unDuration :: Duration -> (Ratio Integer, Integer)
unDuration (Dur r d) = (r,d)


nr :: Integral a => Ratio a -> (a,a)
nr a = (numerator a, denominator a)


-- | Simplify a duration into \simple\ duration plus dotting count.
-- This can be used for matching. 
simplifyDuration :: Duration -> (Duration, Integer)
simplifyDuration d = let (r,dots) = unDuration d in (Dur r 0, dots)

-}
        
        
{-
-- | Augment the duration with a dot.
dot :: Duration -> Duration
dot (Dur r d)       = Dur r (d+1)
         
-- | Augment the duration with double dots.
dotdot :: Duration -> Duration
dotdot (Dur r d)    = Dur r (d+2)  

-- | Augment the duration with triple dots.
dotdotdot :: Duration -> Duration
dotdotdot (Dur r d) = Dur r (d+3)

-- | @'calculateTicks' tpqn dur@ - calculate the integer duration respective
-- to the ticks-per-quarter-note @tqpn@. 
calculateTicks :: Integer -> Duration -> Integer
calculateTicks tpqn dur = 
  let r     = rationalize dur
      (n,d) = (numerator r, denominator r)
  in (tpqn * 4 * n) `div` d

-- | @'rationalize'@ - turn a duration into a ratio which may normalize it. 
rationalize :: Duration -> Rational
rationalize (Dur r n) = dotr r (n,r)
  where
    dotr a (i,r) | i < 1      = a 
                 | otherwise  = dotr (a + r/2) (i-1, r/2)

-- | @'durationSize'@ - size of the duration as a Double.
durationSize :: Duration -> Double
durationSize dur = 
  let r     = rationalize dur
      (n,d) = (fromIntegral $ numerator r, fromIntegral $ denominator r)
  in n / d


-}



--------------------------------------------------------------------------------
-- Instances


instance Affi Duration where
    affi d = shows d    

{-

-- Now that Duration type tracks dots until 'rendering' time
-- arithmetic isn't simple.
   
instance Num Duration where
    (+) (Dur a) (Dur b)   = Dur $ a + b
    (-) (Dur a) (Dur b)   = Dur $ a - b
    (*) (Dur a) (Dur b)   = Dur $ a * b
    
    signum (Dur x)        = Dur (signum x)
    abs (Dur x)           = Dur (abs x)
  
    fromInteger x         = Dur $ fromInteger x


instance Real Duration where  
  toRational              = toRational . unDur

instance Fractional Duration where
    fromRational r        = Dur $ fromRational r
    
    (/) (Dur a) (Dur b)   = Dur $ a / b
    
instance RealFrac Duration where  
  properFraction (Dur a)  =  let (i,f) = properFraction a in (i, Dur f)

-}


 