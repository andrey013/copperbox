{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Core
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Common functions operating on core types
--
--------------------------------------------------------------------------------

module Mullein.Core where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils

import qualified Data.Map as Map
import Data.Ratio
import Data.Sequence ( (|>) )
import qualified Data.Sequence as S



--------------------------------------------------------------------------------
-- Note lists




note :: Pitch -> Duration -> NoteList -> NoteList
note p d t = t |> (Note p d)

rest :: Duration -> NoteList -> NoteList
rest d t = t |> (Rest d)

root :: NoteList
root = S.empty


--------------------------------------------------------------------------------
-- structured /sections/.



--------------------------------------------------------------------------------
-- aggregate sections



-- Do automatic coercion on snoc-ing...
class Snoc c c' where
  (|>>) :: c a -> c' a -> Aggregate a

instance Snoc Section Section where
  (|>>) a b = Literal a :>> Literal b
  
instance Snoc Aggregate Aggregate where
  (|>>) a b = a :>> b
  
instance Snoc Section Aggregate where
  (|>>) a b = Literal a :>> b
    
instance Snoc Aggregate Section where
  (|>>) a b = a :>> Literal b
  
repeated :: Section a -> Aggregate a
repeated = Repeated

keyChange :: Key -> Aggregate a
keyChange = KeyChange



--------------------------------------------------------------------------------
-- Musical representation




--------------------------------------------------------------------------------
-- Meter

meterFraction :: Meter -> Duration
meterFraction (TimeSig n d) = n%d
meterFraction CommonTime    = 4%4 
meterFraction CutTime       = 2%2 
           

metricalSpec :: Integral a => a -> a -> MetricalSpec
metricalSpec n d 
      | compoundMeter  n d  = (time_sig, replicate 3 $ (rational n d) / 3)
      | simpleMeter n d     = (time_sig, replicate (fromIntegral n) (rational 1 d))
      | otherwise           = error $ err_msg
  where
    time_sig = TimeSig (fromIntegral n) (fromIntegral d)
 
    err_msg = "simpleMetricalSpec - can't generate a meter pattern for a "
           ++ "meter that is neither simple or compound."



-- Note compoundMeter and simpleMeter overlap

compoundMeter :: Integral a => a -> a -> Bool
compoundMeter n d = log2whole d && (n `mod` 3 == 0)
         
simpleMeter :: Integral a => a -> a -> Bool
simpleMeter _ d = log2whole d

log2whole :: Integral a => a -> Bool
log2whole = (==0) . snd . pf . logBase 2 . fromIntegral where
    pf :: Double -> (Int, Double)
    pf = properFraction
        
--------------------------------------------------------------------------------
-- pitch labels        

-- Cancel the accidental if the pitch is found in the label set
-- This is the transformation needed for Abc: 
-- f# should be printed f in g major
naturalize :: LabelSet -> Pitch -> Pitch
naturalize lbls p = maybe p ((flip accidentalConst) Nat) (labelSetFind p lbls)
    

labelSetFind :: Pitch -> LabelSet -> Maybe Pitch
labelSetFind (Pitch l a o) (LabelSet m) = 
    maybe Nothing (fn o) (Map.lookup (semitones l + semitones a) m) 
  where
    fn ove (PitchLabel ltr atl) = Just $ Pitch ltr atl ove

  
