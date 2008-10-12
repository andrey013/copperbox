
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
    
    -- particular durations
    duration_zero, no_duration,
    
    dotn,
    
    
    -- * Helper for ratios
    ratioElements, convRational, convRatio, fork,
    durationToDouble,
   
    base2numbers'inf,
    
    PrintableDuration(..), printableDuration,
    pdElements,
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


type Duration = Rational 



duration_zero :: Duration
duration_zero = 0

no_duration :: Duration
no_duration = 0

dotn :: Int -> Duration -> Duration
dotn i d | i < 1 = d
         | otherwise  = sum $ d : take i (halves'inf d)
    
ratioElements :: Integral a => Ratio a -> (a,a)
ratioElements r = (numerator r, denominator r)

convRational :: Integral a => Rational -> Ratio a
convRational = uncurry (%) . fork fromIntegral . ratioElements

convRatio :: Integral a => Ratio a -> Rational
convRatio = uncurry (%) . fork fromIntegral . ratioElements

durationToDouble :: Duration -> Double
durationToDouble = uncurry (/) . fork fromIntegral . ratioElements


fork :: (a -> b) -> (a,a) -> (b,b)
fork f (a,b) = (f a, f b)
  
   
data PrintableDuration = PrintableDuration { 
    _duration  :: Rational,
    _dot_count :: Int 
  }
  deriving (Eq,Ord)


-- | @'rationalize'@ - turn a PrintableDuration back
-- into a Rational which may normalize it. 
rationalize :: PrintableDuration -> Rational
rationalize (PrintableDuration r n) | n >  0     = sum $ r : unfoldr phi (n,r/2)
                                    | otherwise  = r 
  where 
    phi (0,r) = Nothing
    phi (n,r) = Just (r,(n-1,r/2)) 
    
pdElements :: PrintableDuration -> (Int,Int,Int)  
pdElements (PrintableDuration r dc) = 
  let (n,d) = ratioElements r in (fromIntegral n, fromIntegral d, dc) 


printableDuration :: Rational -> PrintableDuration
printableDuration r  
    | r <= 0    = PrintableDuration duration_zero 0
    | otherwise = if r == rationalize r' then r' else PrintableDuration r 0
  where 
    r' = approximateDuration r 
    
    
approximateDuration :: Duration -> PrintableDuration
approximateDuration r
    | r >= 1    = uncurry PrintableDuration $ dotincrease $ upwardsFind r
    | otherwise = uncurry PrintableDuration $ dotincrease $ downwardsFind r
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
    
    
    reccy :: Rational -> Int -> [Rational] -> Int  
    reccy z i   (x:y:ys) | z+x <= r  && z+x+y >= r = if nearer r (z+x) (z+x+y)
                                                       then i else i+1
                         | otherwise               = reccy (z+x) (i+1) (y:ys)     

    nearer a l r = (a - l) <= (r - a) 
                              
halves'inf :: Rational -> [Rational]
halves'inf r = unfoldr phi r
  where
    phi r = Just (r / 2, r / 2)

-- Note: infinte list                
base2numbers'inf :: [Integer]
base2numbers'inf = unfoldr (\x -> Just (x, x * 2)) 1 



                   

instance Pretty PrintableDuration where
  pretty (PrintableDuration r dc) = let (n,d) = ratioElements r in 
      group $ integer n <> char '/' <> integer d <> text (replicate dc '.') 


ppAltRest ch dur = group $
      char ch <> char '/' <> pretty dur




      
--------------------------------------------------------------------------------
-- Named elements




longa                       :: Duration
longa                       = 4%1

-- $amerdoc
-- American naming.
double_whole                :: Duration
double_whole                =  2%1

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

