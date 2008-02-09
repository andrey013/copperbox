


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Perform
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Interval representation
-- |
--------------------------------------------------------------------------------



module Bala.Base.Interval where

import Bala.Base.PitchRep
import Bala.Base.BaseExtra 

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 


data Interval = Interval {
    arithmetic_distance :: Int,
    semitone_count      :: Int
  }
  deriving (Eq)
  
data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth
                  | Seventh | Octave
  deriving (Eq,Enum,Ord)

data IntervalQuality = Perfect | Major | Minor | Augmented | Diminished
  deriving (Eq,Enum,Ord)

data IntervalDistance = Simple | Compound
  deriving (Eq,Enum,Ord,Show)


type NamedInterval = (IntervalQuality, IntervalSize)

--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------

instance Read Interval where 
  readsPrec _ s = readsParsec readInterval s

instance Read IntervalQuality where 
  readsPrec _ s = readsParsec readIntervalQuality s


instance Read IntervalSize where 
  readsPrec _ s = readsParsec readIntervalSize s
   
readInterval :: Parser Interval
readInterval = namedInterval <$> readIntervalQuality <*> readIntervalSize

readIntervalQuality :: Parser IntervalQuality
readIntervalQuality = letter <$> oneOf "PMmAd"
  where 
    letter 'P' = Perfect
    letter 'M' = Major
    letter 'm' = Minor
    letter 'A' = Augmented
    letter 'd' = Diminished

readIntervalSize :: Parser IntervalSize
readIntervalSize = number <$> digit
  where
    number '1' = Unison
    number '2' = Second
    number '3' = Third
    number '4' = Fourth
    number '5' = Fifth
    number '6' = Sixth
    number '7' = Seventh
    number '8' = Octave

--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

-- TODO - handle intervals without names
instance Show Interval where
  showsPrec _ a = let (qy,sz) = intervalName a
                  in shows qy . shows sz

instance Show IntervalQuality where
  showsPrec _ Perfect     = showChar 'P'
  showsPrec _ Major       = showChar 'M'
  showsPrec _ Minor       = showChar 'm'
  showsPrec _ Augmented   = showChar 'A'
  showsPrec _ Diminished  = showChar 'd'
  
instance Show IntervalSize where
  showsPrec _ Unison  = showChar '1'
  showsPrec _ Second  = showChar '2'
  showsPrec _ Third   = showChar '3'
  showsPrec _ Fourth  = showChar '4'
  showsPrec _ Fifth   = showChar '5'
  showsPrec _ Sixth   = showChar '6'
  showsPrec _ Seventh = showChar '7'
  showsPrec _ Octave  = showChar '8'
        


    
--------------------------------------------------------------------------------

arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance (Pitch p1 a1 o1 _) (Pitch p2 a2 o2 _) =
    countingDistance p1 p2 + octaveDistance o1 o2  

countingDistance :: PitchLetter -> PitchLetter -> Int
countingDistance a b = 1 + mod7 (7 + fromEnum b - fromEnum a)

octaveDistance :: Int -> Int -> Int
octaveDistance o1 o2 | o1 == o2 = 0
                     | o1 <  o2 = (o2 - o1) * 7 
                     | o1 >  o2 = (o2 - o1) * 7 - 2
                     
intervalQuality :: Pitch -> Pitch -> Int
intervalQuality p1 p2 = semis p2 - semis p1

 
-- mspan 
mspan :: Pitch -> Pitch -> Int
mspan pch1 pch2 = 1 + mod7 (7 + p2 - p1) + (7 * od)
  where fn = fromEnum . pitch
        p1 = fn pch1
        p2 = fn pch2
        od = octave pch2 - octave pch1
         
intervalName :: Interval -> NamedInterval
intervalName (Interval 1 0)   = (Perfect, Unison)
intervalName (Interval 2 1)   = (Minor, Second)
intervalName (Interval 2 2)   = (Major, Second)
intervalName (Interval 3 3)   = (Minor, Third)
intervalName (Interval 3 4)   = (Major, Third)
intervalName (Interval 4 5)   = (Perfect, Fourth)
intervalName (Interval 4 6)   = (Augmented, Fourth)
intervalName (Interval 5 6)   = (Perfect, Fifth)
intervalName (Interval 5 8)   = (Augmented, Fifth)
intervalName (Interval 6 8)   = (Minor, Sixth)
intervalName (Interval 6 9)   = (Major, Sixth)
intervalName (Interval 7 10)  = (Minor, Seventh)
intervalName (Interval 7 11)  = (Major, Seventh)
intervalName (Interval 8 12)  = (Perfect, Octave)
intervalName _                = undefined

namedInterval :: IntervalQuality -> IntervalSize -> Interval
namedInterval Perfect   Unison  = Interval 1 0
namedInterval Minor     Second  = Interval 2 1
namedInterval Major     Second  = Interval 2 2
namedInterval Minor     Third   = Interval 3 3
namedInterval Major     Third   = Interval 3 4
namedInterval Perfect   Fourth  = Interval 4 5  
namedInterval Augmented Fourth  = Interval 4 6
namedInterval Perfect   Fifth   = Interval 5 6
namedInterval Augmented Fifth   = Interval 5 8
namedInterval Minor     Sixth   = Interval 6 8
namedInterval Major     Sixth   = Interval 6 9
namedInterval Minor     Seventh = Interval 7 10
namedInterval Major     Seventh = Interval 7 11
namedInterval Perfect   Octave  = Interval 8 12
namedInterval _         _       = undefined

