
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
  -- * Datatype
  Duration, 
  
  -- * Destructor
  unDuration, simplifyDuration,
  
 
  
  -- * Operations
  dot, dotdot, dotdotdot,
  
  calculateTicks,
    
  -- * Named instances (American)
  -- $amerdoc
  double_whole, whole, half, quarter, eighth, sixteenth, thirty_second,
  sixty_fourth, one_hundred_twenty_eighth,
  
  -- * Named instances (English)
  -- $engdoc
  breve, semibreve, minim, crochet, quaver, semiquaver, demisemiquaver,
  hemidemisemiquaver, semihemidemisemiquaver, 
  
  longa

  ) where

import Bala.Base.BaseExtra  
import Data.Ratio


data Duration = Dur { 
    ratio :: Ratio Integer, 
    dots  :: Integer
  }
  deriving (Eq,Ord)


-- | Extract a \simple\ ratio plus dotting count 
unDuration :: Duration -> (Ratio Integer, Integer)
unDuration (Dur r d) = (r,d)


nr :: Integral a => Ratio a -> (a,a)
nr a = (numerator a, denominator a)


-- | Simplify a duration into \simple\ duration plus dotting count.
-- This can be used for matching. 
simplifyDuration :: Duration -> (Duration, Integer)
simplifyDuration d = let (r,dots) = unDuration d in (Dur r 0, dots)



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

rationalize :: Duration -> Ratio Integer
rationalize (Dur r n) = dotr r (n,r)
  where
    dotr a (i,r) | i < 1      = a 
                 | otherwise  = dotr (a + r/2) (i-1, r/2)



--------------------------------------------------------------------------------
-- Instances



instance Show Duration where
    showsPrec p d   = showsPrec p (rationalize d)

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



    
    
--------------------------------------------------------------------------------
-- Named elements

durZero :: Ratio Integer -> Duration
durZero r = Dur r 0

-- $amerdoc
-- American naming.
double_whole                :: Duration
double_whole                = durZero (2%1)

whole                       :: Duration
whole                       = durZero (1%1)

half                        :: Duration
half                        = durZero (1%2)

quarter                     :: Duration
quarter                     = durZero (1%4)

eighth                      :: Duration
eighth                      = durZero (1%8)

sixteenth                   :: Duration
sixteenth                   = durZero (1%16)

thirty_second               :: Duration
thirty_second               = durZero (1%32)

sixty_fourth                :: Duration
sixty_fourth                = durZero (1%64)

one_hundred_twenty_eighth   :: Duration
one_hundred_twenty_eighth   = durZero (1%128)

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


longa                       :: Duration
longa                       = durZero (4%1)
  
  