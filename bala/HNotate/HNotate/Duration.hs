
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
    Duration(..), duration, durationElements,

    -- * Operations  
    dot, rationalize, durationToDouble, approxDuration,
    
    base2number_sequence,
    
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



import Data.List (unfoldr)
import Data.Monoid
import Data.Ratio
import Text.PrettyPrint.Leijen hiding (dot)




data Duration = Duration { 
    _duration  :: Ratio Int,
    _dot_count :: Int 
  }
  deriving (Eq,Ord)

-- Shorthand constructor  
duration :: Int -> Int ->  Duration
duration n d = Duration (n%d) 0 

instance Show Duration where
  show = show . pretty

instance Monoid Duration where
  mempty = Duration (fromInteger 0) 0 
  mappend = (+)

durationElements :: Duration -> (Int,Int,Int)  
durationElements (Duration r dc) = let (n,d) = (numerator r, denominator r)
    in (n,d,dc) 

    
    
ratioElements :: Integral a => Ratio a -> (a,a)
ratioElements r = (numerator r, denominator r)

-- | Augment the duration with a dot.
dot :: Duration -> Duration
dot (Duration r d) = Duration r (d+1)

-- | @'rationalize'@ - turn a duration into a ratio which may normalize it. 
rationalize :: Duration -> Ratio Int
rationalize (Duration r dc) = dotr r (dc,r)
  where
    dotr a (i,r) | i < 1      = a 
                 | otherwise  = dotr (a + r/2) (i-1, r/2)
                 
durationToDouble :: Duration -> Double
durationToDouble drn = 
    let r     = rationalize drn
        (n,d) = ratioElements r
    in (fromIntegral n) / (fromIntegral d)
                
                 
approxDuration :: Ratio Int -> Duration
approxDuration r = let (n,d) = ratioElements r in 
    if r < (2%1) then recsmall n d 0 else reclarge r (Duration (closest r) 0)
  where
    recsmall n d dots 
        | n == 0 || d == 0  = Duration 0 0
        | n == 1            = Duration (n%d) dots
        | otherwise         = let (n',d') = ratioElements $ (n%d) - (1%d)
                              in recsmall n' d' (dots + 1)
     
    reclarge r dur = case rationalize (dot dur) `compare` r  of
                        EQ -> dur
                        GT -> Duration r 0 -- a nonstandard duration 
                        LT -> reclarge r (dot dur)
                        
    closest r = let ls = map (flip (%) 1) base2number_sequence
                in last $ takeWhile (r>=) ls
                
base2number_sequence :: [Int]
base2number_sequence = unfoldr (\x -> Just (x, x * 2)) 1 
             

operate op d1 d2 = let r = rationalize d1 `op` rationalize d2
                   in approxDuration r

instance Num Duration where
  d1 + d2 = operate (+) d1 d2
  d1 - d2 = operate (-) d1 d2
  d1 * d2 = operate (*) d1 d2
  negate      = approxDuration . negate . rationalize
  fromInteger = approxDuration . fromInteger
  signum      = approxDuration . signum . rationalize
  abs         = approxDuration . abs . rationalize




instance Pretty Duration where
  pretty drn = let (n,d,dc) = durationElements drn in 
      group $ int n <> char '/' <> int d <> text (replicate dc '.') 


ppAltRest ch dur = group $
      char ch <> char '/' <> pretty dur

      
--------------------------------------------------------------------------------
-- Named elements

durZero :: Ratio Int -> Duration
durZero r = Duration r 0


longa                       :: Duration
longa                       = durZero $ 4%1

-- $amerdoc
-- American naming.
double_whole                :: Duration
double_whole                = durZero $ 2%1

whole                       :: Duration
whole                       = durZero $ 1%1

half                        :: Duration
half                        = durZero $ 1%2

quarter                     :: Duration
quarter                     = durZero $ 1%4

eighth                      :: Duration
eighth                      = durZero $ 1%8

sixteenth                   :: Duration
sixteenth                   = durZero $ 1%16

thirty_second               :: Duration
thirty_second               = durZero $ 1%32

sixty_fourth                :: Duration
sixty_fourth                = durZero $ 1%64

one_hundred_twenty_eighth   :: Duration
one_hundred_twenty_eighth   = durZero $ 1%128

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

