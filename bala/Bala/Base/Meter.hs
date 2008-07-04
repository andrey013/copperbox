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

module Bala.Base.Meter where

import Bala.Base.BaseExtra

import Data.Bits
import Data.Ratio

  
data Meter = Meter { 
    meter_numerator   :: Int,
    meter_denominator :: Int
  }
  deriving (Eq,Show,Read)

{-
data MeasureValue = DoubleWhole | Whole | Half | Quarter | Eighth | Sixteenth
                  | ThirtySecond | SixtyFourth 
  deriving (Eq,Enum,Show,Read)
  
data Duration = Unit MeasureValue 
              | Dotted MeasureValue 
              | Tied Duration Duration
  deriving (Eq,Show,Read)

-}

  
{-
instance Real Meter where
  toRational (Meter n d) = (%) n d
-}

meterRatio :: Meter -> Ratio Int
meterRatio (Meter n d) = (%) n d


  
-- | smart constructor
meter :: Int -> Int -> Meter
meter i j = if (power2 j) then Meter i j else error msg
 where msg = "Cannot create a meter where the denominator is not a power of 2"

{-
-- | \smart enum\ enum on the measures size not its index
measureValue :: Int -> MeasureValue
measureValue i | power2 i  = (toEnum . (+1) . fromIntegral . fn) i
               | otherwise = error msg     
  where 
    fn  = (flip (countTo (flip (/)  2.0))  1.0) . fromIntegral
    msg = "Cannot create a measured value where the denominator is not a power of 2"

-}


power2 :: Int -> Bool
power2 0 = False
power2 i = (i .&. (i - 1)) == 0 


simpleDuple     = meter 2
simpleTriple    = meter 3
simpleQuadruple = meter 4
simpleSextuple  = meter 6


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



perfect :: Meter -> Bool
perfect (Meter i j) = (i `reduces` 2) || (i `reduces` 3)

imperfect :: Meter -> Bool
imperfect = not . perfect

reduces :: (Integral a) => a -> a -> Bool
reduces i j | i == j    = True
            | otherwise = case i `divMod` j of
                            (i',0) -> reduces i' j
                            _      -> False

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

              