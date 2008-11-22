{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


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
import Data.Sequence


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
  

instance Fits Duration Duration where
  measure d     = d  
  resizeTo _  d = d


-- barfill is analoguous to divMod, but with funnier types...

divModBar :: Duration -> TimeSig -> (Int,Duration)
divModBar dn (n,d) = fn $ dn `divModR` (makeDuration n d) where
  fn (i,r) = (fromIntegral i,r)





segmentByTS :: Fits a Duration => TimeSig -> Seq a -> Seq (Seq a)
segmentByTS (n,d) se = segment (makeDuration 1 d) se  




-- Does an event 'sound'? 
-- ... A note or a chord sounds but a rest doesn't.
 
-- Helps for printing metrical bit patterns as x.x...x...


class Sounds a where sounds :: a -> Bool

-- Its arbitrary whether we choose Sounds of Rests for this instance
instance Sounds Duration where
  sounds _ = True
  
  
-- Sounding events with a duration... 


data RhythmicEvent = Sounds Duration | Rests Duration
  deriving (Eq,Show) 

-- obvious instances
instance Sounds RhythmicEvent where
  sounds (Sounds _) = True 
  sounds (Rests _)  = False

instance RhythmicValue RhythmicEvent where
  rhythmicValue (Sounds d)        = d
  rhythmicValue (Rests d)         = d
  
  modifyDuration (Sounds _)     d = Sounds d 
  modifyDuration (Rests _)      d = Rests d 

instance Fits RhythmicEvent Duration where
  measure (Sounds d)            = d
  measure (Rests d)             = d
  
  resizeTo (Sounds _)         d = Sounds d
  resizeTo (Rests _)          d = Rests d
  

  

revent :: (Sounds a, RhythmicValue a) => a -> RhythmicEvent
revent a = if sounds a then Sounds d else Rests d where
    d = rhythmicValue a 



rhythmicEvents :: 
    (Functor t, Sounds a, RhythmicValue a) => t a -> t RhythmicEvent
rhythmicEvents = fmap revent


-- Can this lot have a simpler definition now we have RhythmicEvent...
xdot1 :: Sounds a => a -> Char
xdot1 a | sounds a    = 'x'
        | otherwise   = '.'

instance Sounds Bool where
  sounds = id
  
drawSounds :: (Fits a Duration, Sounds a) => TimeSig -> Seq a -> [Char]
drawSounds tm se = 
    F.foldr (\e a -> (xdot1 $  aggregateSounds1 e) : a) [] (segmentByTS tm se)

  
aggregateSounds1 :: (Fits a Duration, Sounds a) => Seq a -> Bool
aggregateSounds1 se = gteHalf (sumSounds se) (sumMeasure se) where
  sumSounds = F.foldl fn duration_zero  

  fn a e | sounds e   = a + measure e
         | otherwise  = a
  
  gteHalf :: Duration -> Duration -> Bool
  gteHalf a b = a >= (b / 2)
   