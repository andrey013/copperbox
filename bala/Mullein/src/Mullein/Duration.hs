{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Duration
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Duration representation.
--
--------------------------------------------------------------------------------

module Mullein.Duration ( 
  Duration,
  Meter(..),

  Temporal(..),
  Spacer(..),
  
  dot, dotn,

  AugDuration(..),
  rationalize,
  pdElements,
  augDuration,
  ppDuration,

  ) where


import Data.List (unfoldr)
import Data.Ratio

import Text.PrettyPrint.Leijen hiding ( dot )

type Duration = Rational 


-- For /universality/ meter is defined according to Abc's representation.
-- LilyPond will simply generate @TimeSig@ cases.
data Meter = TimeSig Integer Integer 
           -- | CommonTime is 4/4
           | CommonTime 
           -- | CutTime is 2/2
           | CutTime
  deriving (Eq,Show)



class Temporal a where
  duration      :: a -> Duration
  swapDuration  :: Duration -> a -> a

instance Temporal Duration where
  duration      = id
  swapDuration  = const


class Spacer a where
  spacer :: Duration -> a
   

dot :: Duration -> Duration
dot = dotn 1

dotn :: Int -> Duration -> Duration
dotn i d | i < 1 = d
         | otherwise  = d + step i (d /2)
  where
    step 0 _ = 0
    step n d' = d' + step (n-1) (d' / 2)
             
  

--------------------------------------------------------------------------------
-- 



type DotCount = Int 
   
-- Augmented duration - i.e. the ratio is normalized to a /musically useful/ 
-- one and labelled with the dot count
data AugDuration = AugDuration Rational DotCount 
  deriving (Eq,Show)


-- | @'rationalize'@ - turn a AugDuration back into a Rational which 
-- may normalize it. 
rationalize :: AugDuration -> Rational
rationalize (AugDuration r n) | n >  0     = sum $ r : unfoldr phi (n,r/2)
                              | otherwise  = r 
  where 
    phi (0,_) = Nothing
    phi (i,a) = Just (a,(i-1,a/2)) 

    
pdElements :: AugDuration -> (Int,Int,Int)  
pdElements (AugDuration r dc) = 
  (fromIntegral $ numerator r, fromIntegral $ denominator r, dc) 


augDuration :: Duration -> AugDuration
augDuration r  
    | r <= 0    = AugDuration 0 0
    | otherwise = if r == rationalize r' then r' else AugDuration r 0
  where 
    r' = approximateDuration r 
    


    
approximateDuration :: Duration -> AugDuration
approximateDuration drn 
    | drn > 1   = mkApprox (nearestUp drn) 
    | drn < 1   = mkApprox (nearestDown drn)
    | otherwise = AugDuration (1%1) 0
  where
    mkApprox :: Rational -> AugDuration
    mkApprox r = AugDuration r (dotting drn r)

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
ppDuration = pretty . augDuration


instance Pretty AugDuration where
  pretty (AugDuration r dc) =  integer (numerator r) 
                            <> char '/' 
                            <> integer (denominator r) 
                            <> text (replicate dc '.') 
      
      
      
