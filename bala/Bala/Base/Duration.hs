
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
  -- * Named instances (American)
  -- $amerdoc
  double_whole, whole, half, quarter, eighth, sixteenth, thirty_second,
  sixty_fourth, one_hundred_twenty_eighth,
  
  -- * Named instances (English)
  -- $engdoc
  breve, semibreve, minim, crochet, quaver, semiquaver, demisemiquaver,
  hemidemisemiquaver, semihemidemisemiquaver, 
  
  -- * Operations
  dot, dotdot, dotdotdot,
  
  calculateTicks

  ) where
  
import Data.Ratio


type Duration = Ratio Integer

-- $amerdoc
-- American naming.
double_whole                :: Duration
double_whole                = (2%1)

whole                       :: Duration
whole                       = (1%1)

half                        :: Duration
half                        = (1%2)

quarter                     :: Duration
quarter                     = (1%4)

eighth                      :: Duration
eighth                      = (1%8)

sixteenth                   :: Duration
sixteenth                   = (1%16)

thirty_second               :: Duration
thirty_second               = (1%32)

sixty_fourth                :: Duration
sixty_fourth                = (1%64)

one_hundred_twenty_eighth   :: Duration
one_hundred_twenty_eighth   = (1%128)

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

-- | Augment the duration with a dot.
dot :: Duration -> Duration
dot r = r + (r/2)   
         
-- | Augment the duration with double dots.
dotdot :: Duration -> Duration
dotdot r = r + (3 * r / 4)   

-- | Augment the duration with triple dots.
dotdotdot :: Duration -> Duration
dotdotdot r = r + (7 * r / 8)

-- | @'calculateTicks' tpqn dur@ - calculate the integer duration respective
-- to the ticks-per-quarter-note @tqpn@. 
calculateTicks :: Duration -> Duration -> Integer
calculateTicks tpqn r = floor $ tpqn * r

