
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

  ) where

import Bala.Base.BaseExtra  
import Data.Ratio


newtype Duration = Dur { unDur :: Ratio Integer }
  deriving (Eq,Ord)


-- | Augment the duration with a dot.
dot :: Duration -> Duration
dot (Dur r) = Dur $ r + (r/2)   
         
-- | Augment the duration with double dots.
dotdot :: Duration -> Duration
dotdot (Dur r) = Dur $ r + (3 * r / 4)   

-- | Augment the duration with triple dots.
dotdotdot :: Duration -> Duration
dotdotdot (Dur r) = Dur $ r + (7 * r / 8)

-- | @'calculateTicks' tpqn dur@ - calculate the integer duration respective
-- to the ticks-per-quarter-note @tqpn@. 
calculateTicks :: Integer -> Duration -> Integer
calculateTicks tpqn (Dur r) = let (n,d) = (numerator r, denominator r)
  in (tpqn * 4 * n) `div` d


--------------------------------------------------------------------------------
-- Instances



instance Show Duration where
    showsPrec p (Dur r)   = showsPrec p r
  
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

--------------------------------------------------------------------------------
-- Named elements

-- $amerdoc
-- American naming.
double_whole                :: Duration
double_whole                = Dur (2%1)

whole                       :: Duration
whole                       = Dur (1%1)

half                        :: Duration
half                        = Dur (1%2)

quarter                     :: Duration
quarter                     = Dur (1%4)

eighth                      :: Duration
eighth                      = Dur (1%8)

sixteenth                   :: Duration
sixteenth                   = Dur (1%16)

thirty_second               :: Duration
thirty_second               = Dur (1%32)

sixty_fourth                :: Duration
sixty_fourth                = Dur (1%64)

one_hundred_twenty_eighth   :: Duration
one_hundred_twenty_eighth   = Dur (1%128)

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


instance Affi Duration where
    affi d = shows d

  
  
  