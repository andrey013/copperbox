
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
    Duration(..), 
    -- shorthand constructors
    duration, durationR,
    -- destructors
    durationElements,
    
    -- particular durations
    duration_zero, no_duration,
    
    -- * Helper for ratios
    ratioElements, convRational, fork,
    
    -- * Operations  
    dot, dotconst, rationalize, durationToDouble, 
    
    rationalToDuration,
   
    base2numbers'inf,
    
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
duration :: Ratio Int -> Int -> Duration
duration r dc = Duration r dc 

durationR :: Ratio Int -> Duration
durationR r = Duration r 0 

instance Show Duration where
  show = show . pretty

instance Monoid Duration where
  mempty = Duration (fromInteger 0) 0 
  mappend = (+)

durationElements :: Duration -> (Int,Int,Int)  
durationElements (Duration r dc) = let (n,d) = ratioElements r in (n,d,dc) 

duration_zero :: Duration
duration_zero = mempty

no_duration :: Duration
no_duration = mempty {_dot_count = minBound}
    
ratioElements :: Integral a => Ratio a -> (a,a)
ratioElements r = (numerator r, denominator r)

convRational :: Integral a => Rational -> Ratio a
convRational = uncurry (%) . fork fromIntegral . ratioElements

fork :: (a -> b) -> (a,a) -> (b,b)
fork f (a,b) = (f a, f b)
  
   
-- | Augment the duration with a dot.
dot :: Duration -> Duration
dot (Duration r dc) = Duration r (dc+1)

-- | Set the dot value (as per const this forgets the original value).
dotconst :: Duration -> Int -> Duration
dotconst (Duration r _) dc = Duration r dc


-- | @'rationalize'@ - turn a duration into a ratio which may normalize it. 
rationalize :: Duration -> Ratio Int
rationalize (Duration r n) | n >  0     = sum $ r : unfoldr phi (n,r/2)
                           | otherwise  = r 
  where 
    phi (0,r) = Nothing
    phi (n,r) = Just (r,(n-1,r/2)) 
    
    
durationToDouble :: Duration -> Double
durationToDouble = uncurry (/) . fork fromIntegral . ratioElements . rationalize

                

-- maybe durations should be ratios until printing?

rationalToDuration :: Rational -> Duration
rationalToDuration = nonapproxDuration . convRational
  --  ratioToDuration . uncurry (%) . fork fromIntegral . ratioElements 

nonapproxDuration :: Ratio Int -> Duration
nonapproxDuration r  
    | r <= 0    = duration_zero
    | otherwise = if r == rationalize r' then r' else Duration r 0
  where 
    r' = approximateDuration r   

approximateDuration :: Ratio Int -> Duration
approximateDuration r
    | r >= 1%1  = uncurry Duration $ dotincrease $ upwardsFind r
    | otherwise = uncurry Duration $ dotincrease $ downwardsFind r
  where
    upwardsFind r           = upwardsNext r $ map (%1) base2numbers'inf
    
    -- upwardsNext needs lookahead - for (5%1) we want to stop on (4%1) 
    -- rather than go to (8%1) 
    upwardsNext a (x:y:ys)  | a >= x && a < y = x
                            | otherwise       = upwardsNext a (y:ys)
    
    downwardsFind r         = downwardsNext1 r $ map (1%) base2numbers'inf
    
    downwardsNext1 a (x:xs) | a >= x          = x
                            | otherwise       = downwardsNext1 a xs
                            
    dotincrease r' | r' == r   = (r',0)
                   | otherwise = let dc = reccy r' 1 (halves'inf r') in (r',dc)
    
    
    reccy :: Ratio Int -> Int -> [Ratio Int] -> Int  
    reccy z i   (x:y:ys) | z+x <= r  && z+x+y >= r = if nearer r (z+x) (z+x+y)
                                                       then i else i+1
                         | otherwise               = reccy (z+x) (i+1) (y:ys)     

    nearer a l r = (a - l) <= (r - a) 
                              
halves'inf :: Ratio Int -> [Ratio Int]
halves'inf r = unfoldr phi r
  where
    phi r = Just (r / 2, r / 2)

-- Note: infinte list                
base2numbers'inf :: [Int]
base2numbers'inf = unfoldr (\x -> Just (x, x * 2)) 1 




instance Num Duration where
  d1 + d2 = operate (+) d1 d2
  d1 - d2 = operate (-) d1 d2
  d1 * d2 = operate (*) d1 d2
  negate (Duration r dc)    = Duration (negate r) dc
  fromInteger i             = let r = fromInteger i in Duration r 0
  signum (Duration r dc)    = Duration (signum r) dc
  abs (Duration r dc)       = Duration (abs r) dc

{-
instance Integral Duration where
  quotRem = operate quotRem
  toInteger = uncurry div . fork fromIntegral . ratioElements . rationalize
-}

instance Fractional Duration where
  (/) = operate (/)
  fromRational = rationalToDuration

  
  

operate :: (Ratio Int -> Ratio Int -> Ratio Int) 
        -> Duration -> Duration -> Duration
operate op d1 d2 = let r = rationalize d1 `op` rationalize d2
                   in duration r 0
                   

instance Pretty Duration where
  pretty drn = let (n,d,dc) = durationElements drn in 
      group $ int n <> char '/' <> int d <> text (replicate dc '.') 


ppAltRest ch dur = group $
      char ch <> char '/' <> pretty dur




      
--------------------------------------------------------------------------------
-- Named elements




longa                       :: Duration
longa                       = durationR $ 4%1

-- $amerdoc
-- American naming.
double_whole                :: Duration
double_whole                = durationR $ 2%1

whole                       :: Duration
whole                       = durationR $ 1%1

half                        :: Duration
half                        = durationR $ 1%2

quarter                     :: Duration
quarter                     = durationR $ 1%4

eighth                      :: Duration
eighth                      = durationR $ 1%8

sixteenth                   :: Duration
sixteenth                   = durationR $ 1%16

thirty_second               :: Duration
thirty_second               = durationR $ 1%32

sixty_fourth                :: Duration
sixty_fourth                = durationR $ 1%64

one_hundred_twenty_eighth   :: Duration
one_hundred_twenty_eighth   = durationR $ 1%128

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

