


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
import Bala.Base.PitchClass
import Bala.Base.BaseExtra 

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec hiding (token)


data Interval = Interval {
    arithmetic_distance :: Int,
    semitone_count      :: Int
  }
  deriving (Eq)

type NamedInterval = (IntervalQuality, IntervalSize)

  
data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth
                  | Seventh | Octave
  deriving (Eq,Enum,Ord,Show)

data IntervalQuality = Perfect | Major | Minor | Augmented | Diminished
  deriving (Eq,Enum,Ord)

data IntervalDistance = Simple | Compound Int
  deriving (Eq,Ord,Show)





        


    
--------------------------------------------------------------------------------
-- | count of 'letter names' inclusive between two pitches (ordered,multioctave)
arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance (Pitch p1 a1 o1 _) (Pitch p2 a2 o2 _) =
    letterDist p1 p2 + oveDist o1 o2  
  where
    letterDist :: PitchLetter -> PitchLetter -> Int
    letterDist a b = 1 + mod7 (7 + fromEnum b - fromEnum a)
    
    oveDist :: Int -> Int -> Int
    oveDist o1 o2 | o1 == o2 = 0
                  | o1 <  o2 = (o2 - o1) * 7 
                  | o1 >  o2 = (o2 - o1) * 7 - 2

-- | count of semitones between two pitches (ordered,multioctave)                    
semitoneDistance :: Pitch -> Pitch -> Int
semitoneDistance p1 p2 = semis p2 - semis p1

-- | synonym of semitoneDistance
orderedPitchInterval :: Pitch -> Pitch -> Int
orderedPitchInterval = semitoneDistance


-- | absolute value of semitone distance
unorderedPitchInterval :: Pitch -> Pitch -> Int
unorderedPitchInterval p1 p2 = abs $ semitoneDistance p1 p2


-- | count of semitones between two pitches (ordered,single-octave)
orderedPCInterval :: PC -> PC -> Int
orderedPCInterval (PC p1) (PC p2) = p2 - p1 

-- http://www.music.iastate.edu/courses/337/PostTonalTerms.html
-- http://www.robertkelleyphd.com/atnltrms.htm
-- | count of semitones between two pitches (unordered,single-octave)                    
unorderedPCInterval (PC p1) (PC p2)
  | p1 < p2   = p2 - p1
  | otherwise = p1 - p2 

-- countingDistance' :: PitchLetter -> PitchLetter -> IntervalSize
-- countingDistance' a b = mkIntervalSize $ (fromEnum a - fromEnum b) `mod` 12 --  + mod7 (7 + fromEnum b - fromEnum a)

-- mkIntervalSize :: Int -> IntervalSize
-- mkIntervalSize a = toEnum a -- (a-1)



                     

diatonic, chromatic :: Interval -> Bool
diatonic _ = undefined -- perfect major or minor

chromatic = not . diatonic

intervalClass :: Interval -> Bool
intervalClass = undefined
 
-- mspan 
mspan :: Pitch -> Pitch -> Int
mspan pch1 pch2 = 1 + mod7 (7 + p2 - p1) + (7 * od)
  where fn = fromEnum . pitch
        p1 = fn pch1
        p2 = fn pch2
        od = octave pch2 - octave pch1
         
intervalName :: Interval -> Maybe NamedInterval
intervalName (Interval 1 0)   = Just (Perfect, Unison)
intervalName (Interval 2 1)   = Just (Minor, Second)
intervalName (Interval 2 2)   = Just (Major, Second)
intervalName (Interval 3 3)   = Just (Minor, Third)
intervalName (Interval 3 4)   = Just (Major, Third)
intervalName (Interval 4 5)   = Just (Perfect, Fourth)
intervalName (Interval 4 6)   = Just (Augmented, Fourth)
intervalName (Interval 5 6)   = Just (Perfect, Fifth)
intervalName (Interval 5 8)   = Just (Augmented, Fifth)
intervalName (Interval 6 8)   = Just (Minor, Sixth)
intervalName (Interval 6 9)   = Just (Major, Sixth)
intervalName (Interval 7 10)  = Just (Minor, Seventh)
intervalName (Interval 7 11)  = Just (Major, Seventh)
intervalName (Interval 8 12)  = Just (Perfect, Octave)
intervalName _                = Nothing

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

readIntervalConstr :: Parser Interval
readIntervalConstr = parens inner
  where
    inner = Interval <$> (token (string "Interval") *> token int)
                     <*> token int
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

-- TODO - handle intervals without names
instance Show Interval where
  showsPrec _ a = case intervalName a of
                    Just (qy,sz) -> shows qy . shows (1 + fromEnum sz)
                    Nothing -> ic a 
    where 
      ic (Interval d c) = showParen True $ 
            showString "Interval " . shows d . showSpace . shows c

  
instance Show IntervalQuality where
  showsPrec _ Perfect     = showChar 'P'
  showsPrec _ Major       = showChar 'M'
  showsPrec _ Minor       = showChar 'm'
  showsPrec _ Augmented   = showChar 'A'
  showsPrec _ Diminished  = showChar 'd'
  

  

                    