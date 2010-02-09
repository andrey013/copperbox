{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data types (e.g time signature) for musical structures.
--
--------------------------------------------------------------------------------

module Neume.Datatypes
  (
    MeterPattern
  , makeMeterPattern
  , compoundMeter
  , simpleMeter
  , TimeSignature(..)
  , MetricalSpec(..)
   
  ) where

import Neume.Utils


--------------------------------------------------------------------------------
-- Meter patterns


-- Implementation note - MeterPatterns must support arithmetic
-- so are lists of Rationals rather that the lists of Duration.


type MeterPattern = [Rational] 
     

makeMeterPattern :: Int -> Int -> MeterPattern
makeMeterPattern n d 
      | compoundMeter  n d  = replicate 3 $ (makeRational n d) / 3
      | simpleMeter n d     = replicate n $ makeRational 1 d
      | otherwise           = error $ err_msg
  where
    err_msg = "meterPattern - can't generate a meter pattern for a "
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

-------------------------------------------------------------------------------
-- Time signatures

data TimeSignature = TimeSignature { ts_meter :: Int , ts_pulse :: Int }
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Metrical specification

data MetricalSpec = MetricalSpec { 
        timeSignature :: TimeSignature,
        meterPattern  :: MeterPattern
      }
  deriving (Eq,Show)
