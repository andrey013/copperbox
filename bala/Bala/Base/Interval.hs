


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


data Interval = Interval {
    arithmetic_distance :: Int,
    semitone_count      :: Int
  }
  deriving (Eq,Show)
  
data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth
                  | Seventh | Octave
  deriving (Eq,Enum,Ord,Show)

data IntervalQuality = Perfect | Major | Minor | Augmented | Diminished
  deriving (Eq,Enum,Ord,Show)

data IntervalDistance = Simple | Compound
  deriving (Eq,Enum,Ord,Show)


type NamedInterval = (IntervalQuality, IntervalSize)


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

