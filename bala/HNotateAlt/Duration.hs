
--------------------------------------------------------------------------------
-- |
-- Module      :  Duration
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A minimal representation for Duration.
--
--------------------------------------------------------------------------------

module Duration (
    -- Data type
    Duration(..),

    -- * Operations  
    toRatio, getDuration, toDouble, fromDouble,
    
    -- * Midi helper
    midiTicks,
    
    ppAltRest,

    -- * Named instances (American)
    longa, 
    -- $amerdoc
    double_whole, whole, half, quarter, eighth, sixteenth, thirty_second,
    sixty_fourth, one_hundred_twenty_eighth,
    
    -- * Named instances (English)
    -- $engdoc
    breve, semibreve, minim, crochet, quaver, semiquaver, demisemiquaver,
    hemidemisemiquaver, semihemidemisemiquaver    

  ) where

import Data.Ratio
import Text.PrettyPrint.Leijen



-- TO DO - currently I'm ignoring dotting dotting!
data Duration = Duration { 
    _duration  :: Int,
    _dot_count :: Int 
  }
  deriving (Eq,Ord)

instance Show Duration where
  show = show . pretty
  


-- | Augment the duration with a dot.
dot :: Duration -> Duration
dot (Duration r d) = Duration r (d+1)

                 
-- todo - dots                 
toDouble :: Duration -> Double
toDouble (Duration i dc) = 1.0 / (fromIntegral i)

toRatio (Duration n d) = 1 % (fromIntegral n)

getDuration (Duration n d) = (n,d)

fromDouble :: Double -> Duration
fromDouble j = let r = toRational j; (n,d) = (numerator r, denominator r)
               in if (n==1) then Duration (fromIntegral d) 0
                            else error "fromDouble - to do"
               



midiTicks :: Integer -> Duration -> Integer
midiTicks tpqn d = floor $ fromIntegral (4 * tpqn) * toDouble d


instance Pretty Duration where
  pretty (Duration n dc) = group $ int n <> text (replicate dc '.') 


ppAltRest ch dur = group $
      char ch <> char '/' <> pretty dur
      
--------------------------------------------------------------------------------
-- Named elements

durZero :: Int -> Duration
durZero r = Duration r 0


longa                       :: Duration
longa                       = durZero (-4)

-- $amerdoc
-- American naming.
double_whole                :: Duration
double_whole                = durZero (-2)

whole                       :: Duration
whole                       = durZero 1

half                        :: Duration
half                        = durZero 2

quarter                     :: Duration
quarter                     = durZero 4

eighth                      :: Duration
eighth                      = durZero 8

sixteenth                   :: Duration
sixteenth                   = durZero 16

thirty_second               :: Duration
thirty_second               = durZero 32

sixty_fourth                :: Duration
sixty_fourth                = durZero 64

one_hundred_twenty_eighth   :: Duration
one_hundred_twenty_eighth   = durZero 128

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

