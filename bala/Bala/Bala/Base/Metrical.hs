{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Metrical
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Duration representation - pulls in the HNotate.Duration module
--
--------------------------------------------------------------------------------

module Bala.Base.Metrical where

import Bala.Base.BaseExtra (applyi, log2whole)

import Bala.Base.Duration
import HNotate.Fits

import qualified Data.Foldable as F
import Data.Ratio
import Data.Sequence hiding (length)


type TimeSig = (Int,Int)

-- Should TimeSig be a newtype so we can only build it with this function?
timeSig :: Int -> Int -> TimeSig
timeSig n d | n < 1             = error "timeSig n must be (>=1)"
            | not (log2whole d) = error base_error
            | otherwise         = (n,d)
  where            
    base_error = "timeSig - d must be in the sequence [1,2,4,8,16,..]"

compoundMeter :: TimeSig -> Bool
compoundMeter (n,d) = log2whole d && (n `mod` 3 == 0)

simpleMeter :: TimeSig -> Bool
simpleMeter (_,d)   = log2whole d


unitDuration :: TimeSig -> Duration
unitDuration (n,d) = makeDuration n d

showBars :: Int -> TimeSig -> ShowS
showBars n s = showChar '|' . applyi n (tsRender1 s . showChar '|')


-- O % & , .
-- wn, hn, qn, 8th, 16th  

tsRender1 :: TimeSig -> ShowS
tsRender1 (n,d) = applyi n ((cs d) .) id where
  cs 1  = showChar 'O'
  cs 2  = showChar '%'
  cs 4  = showChar '&'
  cs 8  = showChar ','
  cs 16 = showChar '.'
  cs _  = showChar '+' 
  


-- barfill is analoguous to divMod, but with funnier types...

divModBar :: Duration -> TimeSig -> (Int,Duration)
divModBar dn (n,d) = fn $ dn `divModR` (makeDuration n d) where
  fn (i,r) = (fromIntegral i,r)





segmentByTS :: Fits a Duration => TimeSig -> Seq a -> Seq (Seq a)
segmentByTS (n,d) se = segment False (makeDuration 1 d) se  




-- Does an event 'sound'? 
-- ... A note or a chord sounds but a rest doesn't.
 
-- Helps for printing metrical bit patterns as x.x...x...


class Sounds a where 
  sounds :: a -> Bool
  rest :: Duration -> a
  
  spacer :: Duration -> a
  spacer d = rest d
  
  
  
-- Duration doesn't really have a natural instance for Sounds
-- but it makes things a lot easier if we pretend it does.
instance Sounds Duration where
  sounds _ = True
  rest d = d
  
  
-- Sounding events with a duration... 


data RhythmicEvent = Sounds Duration | Rests Duration
  deriving (Eq,Show) 

-- obvious instances
instance Sounds RhythmicEvent where
  sounds (Sounds _) = True 
  sounds (Rests _)  = False
  
  rest d          = Rests d

instance RhythmicValue RhythmicEvent where
  rhythmicValue (Sounds d)        = d
  rhythmicValue (Rests d)         = d
  
  updateDuration d (Sounds _)     = Sounds d
  updateDuration d (Rests _)      = Rests d


instance Fits RhythmicEvent Duration where
  measure     = rhythmicValue
  split       = splitRV
  

  

revent :: (Sounds a, RhythmicValue a) => a -> RhythmicEvent
revent a = if sounds a then Sounds d else Rests d where
    d = rhythmicValue a 



rhythmicEvents :: 
    (Functor t, Sounds a, RhythmicValue a) => t a -> t RhythmicEvent
rhythmicEvents = fmap revent


 



-- clave patterns can have varoius 'prolongational' interpretations...
-- i.e. is easy to get a clave pattern from a event durational pattern, but 
-- not the other way round.

data Clave = ClaveOn | ClaveOff
  deriving (Eq,Enum,Show)
  
type Bits = [Clave]

newtype ClavePattern = ClavePattern { getClavePattern :: Seq Clave }

    
bjorklund :: Int -> Int -> ClavePattern
bjorklund n k = 
    postP $ step (replicate k [ClaveOn], replicate (n - k) [ClaveOff]) 
  where
    step :: ([Bits], [Bits]) -> Bits
    step (as,rs) 
        | length rs < 2 = concat as ++ concat rs
        | otherwise     = step $ repartition $ (uncurry distribute) (as,rs) 
    
    repartition (as,rs) = let len   = length $ head as
                              (x,y) = span ((==) len . length) as 
                          in (x,rs++y)
                              
    postP = ClavePattern . fromList
    
                      
-- distribute for Bjorklund's algorithm is basically a zip, except we 
-- return the unconsumed right input. (Hence we need to return a pair, 
-- hence we use an accumulator)  
distribute :: [Bits] -> [Bits] -> ([Bits], [Bits])
distribute xs ys = step [] xs ys where

  step :: [Bits] -> [Bits] -> [Bits] -> ([Bits], [Bits])
  step acc (x:xs) (y:ys)  = step (acc ++ [x ++ y]) xs ys
  step acc []     []      = (acc, [])
  step acc []     (y:ys)  = (acc, (y:ys))
  step acc (x:xs) []      = (acc ++ (x:xs), [])
  
  
       