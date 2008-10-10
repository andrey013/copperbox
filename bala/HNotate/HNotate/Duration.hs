
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
    
    -- * Operations  
    dot, dotconst, rationalize, durationToDouble, 
    
    representableDuration, 
    printableDuration, printableDurationF,
    splitDuration,
    
    
    
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
durationElements (Duration r dc) = let (n,d) = (numerator r, denominator r)
    in (n,d,dc) 

duration_zero :: Duration
duration_zero = mempty

no_duration :: Duration
no_duration = mempty {_dot_count = minBound}
    
ratioElements :: Integral a => Ratio a -> (a,a)
ratioElements r = (numerator r, denominator r)

-- | Augment the duration with a dot.
dot :: Duration -> Duration
dot (Duration r dc) = Duration r (dc+1)

-- | Set the dot value (as per const this forgets the original value).
dotconst :: Duration -> Int -> Duration
dotconst (Duration r _) dc = Duration r dc


-- | @'rationalize'@ - turn a duration into a ratio which may normalize it. 
rationalize :: Duration -> Ratio Int
rationalize (Duration r dc)   = dotr r (dc,r)
  where
    dotr a (i,r) | i < 1      = a 
                 | otherwise  = dotr (a + r/2) (i-1, r/2)
                 
durationToDouble :: Duration -> Double
durationToDouble drn = 
    let r     = rationalize drn
        (n,d) = ratioElements r
    in (fromIntegral n) / (fromIntegral d)
                




-- Cannot be longer than 2 x breve -- longest is a breve with inf. dots
representableDuration :: (Duration -> a) -> (Ratio Int -> a) -> Ratio Int -> a
representableDuration succCont failCont r 
    | r > 8%1 || r < (1%128) = failCont r
    | otherwise              = let rt = root r in dot (duration rt) rt 0
                              
  where
    rec_lim       = 3
    root r        = head $ dropWhile (r<) decreasing_ratios
    
    dot sk r' i   | r' == r      = succCont $ sk i
                  | i > rec_lim  = failCont r
                  | otherwise    = dot sk (dotaugment r') (i+1)
                   
    dotaugment r  = r + (1 % (denominator r *2))                 

printableDuration :: Ratio Int -> Maybe Duration
printableDuration = representableDuration Just (const Nothing)

printableDurationF :: Ratio Int -> Duration
printableDurationF r = maybe (errUnRep r) id (printableDuration r)
  where 
    errUnRep r = error $ 
            "printableDurationF - cannot make a representable from " ++ show r
          
-- Note: infinte list 
decreasing_ratios :: [Ratio Int]
decreasing_ratios = 4%1 : 2%1 : xs
  where xs = map (1 %) base2number_sequence

-- Note: infinte list                
base2number_sequence :: [Int]
base2number_sequence = unfoldr (\x -> Just (x, x * 2)) 1 


splitDuration :: Duration -> Duration -> Maybe [Duration]
splitDuration d elt = let (final,xs) = multiElts d [] in 
    maybe Nothing (\e -> Just $ xs ++ [e]) (remake final) 
  where
    multiElts d acc | d > elt   = multiElts (d-elt) (elt:acc)
                    | otherwise = (d,acc)  
  
    remake          = printableDuration . rationalize



instance Num Duration where
  d1 + d2 = operate (+) d1 d2
  d1 - d2 = operate (-) d1 d2
  d1 * d2 = operate (*) d1 d2
  negate (Duration r dc)    = Duration (negate r) dc
  fromInteger i             = let r = fromInteger i in Duration r 0
  signum (Duration r dc)    = Duration (signum r) dc
  abs (Duration r dc)       = Duration (abs r) dc

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

