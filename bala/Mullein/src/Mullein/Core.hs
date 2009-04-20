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
import Mullein.RS
import Mullein.Utils

import Data.Ratio


--------------------------------------------------------------------------------
-- Note lists


type BarNum = Int
type OverlayList e = ([ElementP e], [(BarNum,[ElementP e])])



type NoteCtx a = RS St Env a


-- NoteListCtx represents /shorthand state/ so we can omit
-- some details when building the notelist (e.g. duration) 
data St = St { prev_note_length :: Duration,
               metrical_spec    :: MetricalSpec,
               current_key      :: Key
             }
  deriving (Eq,Show)

data Env = Env {}




--------------------------------------------------------------------------------
-- Musical representation




--------------------------------------------------------------------------------
-- Meter

meterFraction :: Meter -> Duration
meterFraction (TimeSig n d) = n%d
meterFraction CommonTime    = 4%4 
meterFraction CutTime       = 2%2 
           

metricalSpec :: Int -> Int -> MetricalSpec
metricalSpec n d 
      | compoundMeter  n d  = (time_sig, replicate 3 $ (rational n d) / 3)
      | simpleMeter n d     = (time_sig, replicate n (rational 1 d))
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
