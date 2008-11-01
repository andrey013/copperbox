--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Meter
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Meter (currently somewhat subsumed by Duration)
--
--------------------------------------------------------------------------------

module Bala.Base.Meter (
  -- * Datatypes 
  MeterFraction,
  
  -- * Constructors and destructors
  (//), unMeterFraction, meterRatio,
  
  -- * Operations
  meterSize, perfect, imperfect
  
  ) where

import Bala.Base.BaseExtra

import Data.Bits
import Data.Ratio

-- * Meter fraction
-- | An alternative to Data.Rational which normalizes where possible
-- e.g. 4\/4 becomes 1\/1. For time signatures we don't want to normalize.
data MeterFraction = Int :/ Int
  deriving (Eq)


instance Show MeterFraction where
  showsPrec _ (n :/ d) = shows n . showChar '/' . shows d


  
infixl 2 //

-- | smart constructor - replaces (:\/\/).  
(//) :: Integral a => a -> a -> MeterFraction
(//) n d  
    | power2 d    = fromIntegral n :/ fromIntegral d 
    | otherwise   = error msg
  where 
    msg = "Cannot create a meter where the denominator is not a power of 2"


unMeterFraction :: MeterFraction -> (Int,Int)
unMeterFraction (n :/ d) = (n,d)

meterRatio :: MeterFraction -> Ratio Int
meterRatio (n :/ d) = (%) n d


power2 :: Integral a => a -> Bool
power2 0 = False
power2 i = fn $ fromIntegral i
  where
    fn :: Integer -> Bool
    fn i = (i .&. (i - 1)) == 0 


-- | @'meterSize'@ - size (as a Double) of one bar in the given meter.
meterSize :: MeterFraction -> Double
meterSize (n :/ d) = fromIntegral n / fromIntegral d
  


perfect :: MeterFraction -> Bool
perfect (n :/ d) = (n `reduces` 2) || (n `reduces` 3)

imperfect :: MeterFraction -> Bool
imperfect = not . perfect

reduces :: (Integral a) => a -> a -> Bool
reduces i j | i == j    = True
            | otherwise = case i `divMod` j of
                            (i',0) -> reduces i' j
                            _      -> False






simpleDuple     = (//) 2
simpleTriple    = (//) 3
simpleQuadruple = (//) 4
simpleSextuple  = (//) 6


-- Read page 151 ex 10-4

-- see Duckworth page 13
-- each beat divided into two
-- simple duple       2/8   2/4   2/2
-- simple triple      3/8   3/4   3/2
-- simple quadruple   4/8   4/4   4/2


-- see Duckworth page 17
-- each beat divided into 3
-- compound duple     6/16  6/8  6/4
-- compound triple    9/16  9/8  9/4
-- compound quadruple 12/16 12/8 12/4


{-
division :: MeasureValue -> Float
division = (2 **) . negate . fromIntegral . (flip (-) 1) . fromEnum
-}

{-
-- simple and compound are not mutually exclusive 
-- 6/2 - o| o| o| o| o| o| - o| o| o| (>)o| o| o|

simple :: Meter -> Bool
simple (Meter i j) = i `div` 3 > 1 && j `mod` 4 == 0

compound :: Meter -> Bool
compound (Meter i j) = i `div` 3 > 1 && j `mod` 4 == 0


duple :: Meter -> Bool
duple (Meter i j) = even i


triple :: Meter -> Bool
triple (Meter 6 8) = False -- as a triple it would be Meter 3 4
triple (Meter i j) = i `mod` 3 == 0
-}


-- If a compound is divisible 2, second stress is half-way
-- divisible 3, two stresses third and two thirds

{-
divisibleNumerator :: Int -> Meter -> Bool
divisibleNumerator a (Meter b _) = b `mod` a == 0


 
pulse :: Meter -> [Duration]
pulse (Meter n d) = replicate n (durationOf d)

durationOf 1 = Unit Whole
durationOf 2 = Unit Half


generatesSimple :: Meter -> Maybe [MeasureValue]
generatesSimple _ = undefined

-}

              