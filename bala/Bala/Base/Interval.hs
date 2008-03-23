

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
import Bala.Base.PitchOps
import Bala.Base.PitchClass
import Bala.Base.BaseExtra 

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------



data Interval = Interval { 
    arith_dist :: Int, 
    half_steps :: Int 
    }
  deriving (Show,Eq)


  

type NamedInterval = (IntervalQuality, IntervalSize)

newtype IntervalPattern = IntervalPattern [Int]
  deriving (Eq,Show)

newtype ScaleDegreePattern = ScaleDegreePattern [(Int,Accidental)]

  
  
data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth
                  | Seventh | Octave
  deriving (Eq,Enum,Ord,Show)

data IntervalQuality = Perfect | Major | Minor | Augmented | Diminished
  deriving (Eq,Enum,Ord)

data IntervalDistance = Simple | Compound Int
  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
-- Instances for Interval
--------------------------------------------------------------------------------

instance Num Interval where
  
  (Interval d s) + (Interval d' s') = 
    Interval (d `countfwd` d') (s + s')

  (Interval d s) - (Interval d' s') =  
      Interval (d `countbackNZ` d') (s `backtrack` s')
  
  (Interval d s) * (Interval d' s') =  
      Interval (d * d') (s * s')
    
  abs  = id
  
  negate _ = Interval 1 0
  
  signum (Interval d s) = Interval 1 0
  
  fromInteger = fromSemis      
           
instance Semitones Interval where
  semitones (Interval _ sc) = sc
    

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

countfwd :: (Num a) => a -> a -> a
countfwd a i = a + (i - 1)

countback, countbackNZ :: (Num a, Ord a) => a -> a -> a
countback   a i = abs $ a - (i - 1)
countbackNZ a i | a > i     = countback a i
                | otherwise = countback i a

backtrack, backtrackNZ :: (Num a) => a -> a -> a
backtrack a i = abs $ a - i
backtrackNZ a i = 
  let a' = a - i in case signum a' of (-1) -> (abs a') + 1 ; _ -> a'
  
  
--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------



-- Smart constructor for Intervals
-- TODO - Intervals be can constructed if the arithmetic distance and the 
-- semitone count don't make sense 
-- eg (1,8) should only have (1,1) or (1,0)
-- (unless of course wild pitch spelling is considered)

interval d s 
    | d > 0 && s >= 0 = Interval d s
    | d == 0          = error dzero_msg
    | otherwise       = error mixed_msg
 where
  dzero_msg = "Cannot create an Interval with an arithmetic distance of 0"
  mixed_msg = "Cannot create an Interval with negative numbers for\n"
           ++ "arithmetic distance or semitone count" 

-- fromSemis is biased 
-- it produces m6 rather than A5 & A4 and A4 rather than d4
fromSemis i = let i' = fromIntegral (abs i) in interval (stepy i') i'  

stepy :: (Num a, Ord a) => a -> a
stepy i | i < 0       = stp $ abs i
        | otherwise   = stp i
  where
    stp i | i == 0                      = 1
          | i == 1 || i == 2            = 2
          | i == 3 || i == 4 || i == 5  = 3
          | i == 6                      = 4
          | i == 7                      = 5
          | i == 8 || i == 9            = 6
          | i == 10 || i == 11          = 7
          | i == 12                     = 8
          | otherwise                   = 7 + stp (i - 13)
  
    

--------------------------------------------------------------------------------
-- Ord Instance
--------------------------------------------------------------------------------

instance Ord Interval where
  (Interval _ s) `compare` (Interval _ s') = s `compare` s'
  

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------
  
-- | count of 'letter names' inclusive between two pitches (ordered,multioctave)
arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance p1 p2 | p1 > p2   = orderedDist p2 p1
                         | otherwise = orderedDist p1 p2

  where
    orderedDist (Pitch l o s _) (Pitch l' o' s' _) = undefined
{-    
      let (ld,wrapped) = letterDist l1 l2
          ove          = oveDist o1 o2 wrapped
      in ld + ove  
-}    

    oveDist :: Int -> Int -> Bool -> Int
    oveDist o1 o2 True  = 7 * (o2 - o1 - 1)
    oveDist o1 o2 False = 7 * (o2 - o1)
    
    -- calc distance and whether it has wrapped around
    letterDist :: PitchLetter -> PitchLetter -> (Int,Bool)
    letterDist x y = (dist, not (x <= y)) 
      where dist = 1 + mod7 (7 + fromEnum y - fromEnum x)

    
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



                     

diatonicInterval, chromaticInterval :: Interval -> Bool
diatonicInterval _ = undefined -- perfect major or minor

chromaticInterval = not . diatonicInterval

intervalClass :: Interval -> Bool
intervalClass = undefined

         
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

 

                