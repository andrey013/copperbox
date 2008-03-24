

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


data NamedInterval = NamedInterval {
    interval_measure :: IntervalMeasure,
    interval_quality :: IntervalQuality,
    interval_size    :: IntervalSize
  }
  deriving (Eq,Show)  
  


  
data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth
                  | Seventh | Octave
  deriving (Eq,Enum,Ord,Show)

data IntervalQuality = Perfect | Major | Minor | Augmented | Diminished
  deriving (Eq,Enum,Ord,Show)

data IntervalMeasure = Simple | Compound Int
  deriving (Eq,Ord,Show)


-- these look like they should be somewhere else
newtype IntervalPattern = IntervalPattern [Int]
  deriving (Eq,Show)

newtype ScaleDegreePattern = ScaleDegreePattern [(Int,Accidental)]

  
  
  
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
-- Enum 
--------------------------------------------------------------------------------

instance Enum IntervalMeasure where
  fromEnum Simple       = 0 
  fromEnum (Compound i) = i 
  

  toEnum i | i == 0    = Simple
           | i >  0    = Compound i
           | otherwise = Compound (abs i)
           

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


-- | counts from left to right
letterDistance :: PitchLetter -> PitchLetter -> Int
letterDistance l l' = let diff = (fromEnum l') - (fromEnum l) in 
                      if (signum diff == (-1)) then 8 + diff else 1 + diff
  
letterSucc :: Int -> PitchLetter -> PitchLetter
letterSucc i l = successor (i `mod` 8) l

 
-- | number of 'letter names' inclusive between the highest and lowest pitch
arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance p@(Pitch l o _ _)  p'@(Pitch l' o' _ _)
    | p == p'   = 1
    | p < p'    = orderedDist (pitch_letter l, o)   (pitch_letter l', o')
    | otherwise = orderedDist (pitch_letter l', o') (pitch_letter l, o) 
  where
    orderedDist (p,o) (p',o') = let pd = letterDistance p p'
                                in pd + 7 * (o' - o)



buildInterval :: Pitch -> Pitch -> Interval
buildInterval p  p' = 
  interval (p `arithmeticDistance` p') (p `semitoneDistance` p')


invert :: Interval -> Interval
invert (Interval a s) | a < 9     = Interval (9-a) (12-s)
                      | otherwise = error "todo invert on large intervals ..."
 
simplify :: Interval -> (IntervalMeasure,Interval)
simplify (Interval a s) 
  | a > 8 && s > 12 = let (i,s') = s `divMod` 12 
                      in (Compound i, Interval (a `mod` 8) s')
  | otherwise       = (Simple, Interval a s) 



intervalName :: Interval -> Maybe NamedInterval
intervalName invl = let (measure, simple) = simplify invl in fn measure simple
  where     
    fn m (Interval 1 0)   = Just (NamedInterval m Perfect Unison)
    fn m (Interval 2 1)   = Just (NamedInterval m Minor Second)
    fn m (Interval 2 2)   = Just (NamedInterval m Major Second)
    fn m (Interval 3 3)   = Just (NamedInterval m Minor Third)
    fn m (Interval 3 4)   = Just (NamedInterval m Major Third)
    fn m (Interval 4 5)   = Just (NamedInterval m Perfect Fourth)
    fn m (Interval 4 6)   = Just (NamedInterval m Augmented Fourth)
    fn m (Interval 5 6)   = Just (NamedInterval m Perfect Fifth)
    fn m (Interval 5 8)   = Just (NamedInterval m Augmented Fifth)
    fn m (Interval 6 8)   = Just (NamedInterval m Minor Sixth)
    fn m (Interval 6 9)   = Just (NamedInterval m Major Sixth)
    fn m (Interval 7 10)  = Just (NamedInterval m Minor Seventh)
    fn m (Interval 7 11)  = Just (NamedInterval m Major Seventh)
    fn m (Interval 8 12)  = Just (NamedInterval m Perfect Octave)
    fn m _                = Nothing

namedInterval :: NamedInterval -> Maybe Interval
namedInterval (NamedInterval msr qlty sz) = fmap (mk msr) (fn qlty sz)
  where 
    mk Simple       (a,s) = Interval a s
    mk (Compound i) (a,s) = Interval (a + 8 * i) (s + 12 * i)

    fn Perfect   Unison  = Just (1,0)
    fn Minor     Second  = Just (2,1)
    fn Major     Second  = Just (2,2)
    fn Minor     Third   = Just (3,3)
    fn Major     Third   = Just (3,4)
    fn Perfect   Fourth  = Just (4,5) 
    fn Augmented Fourth  = Just (4,6)
    fn Perfect   Fifth   = Just (5,6)
    fn Augmented Fifth   = Just (5,8)
    fn Minor     Sixth   = Just (6,8)
    fn Major     Sixth   = Just (6,9)
    fn Minor     Seventh = Just (7,10)
    fn Major     Seventh = Just (7,11)
    fn Perfect   Octave  = Just (8,12)
    fn  _         _      = Nothing 


extr :: Pitch -> Interval -> Pitch
extr p@(Pitch (PitchLabel l a) o s c) (Interval ad sc) =
    let (oc,s') = explode12 $ s + sc
        l'      = letterSucc (ad - 1) l       
        lbl     = spell (pitch_label $ p `addSemi` sc) l'
    in Pitch lbl (oc + o) s' c

--------------------------------------------------------------------------------
-- old.... 



-- | absolute value of semitone distance
unorderedPitchInterval :: Pitch -> Pitch -> Int
unorderedPitchInterval p1 p2 = abs $ semitones p1 - semitones p2


-- | count of semitones between two pitches (ordered,single-octave)
orderedPCInterval :: PC -> PC -> Int
orderedPCInterval (PC p1) (PC p2) = p2 - p1 

-- http://www.music.iastate.edu/courses/337/PostTonalTerms.html
-- http://www.robertkelleyphd.com/atnltrms.htm
-- | count of semitones between two pitches (unordered,single-octave)                    
unorderedPCInterval (PC p1) (PC p2)
  | p1 < p2   = p2 - p1
  | otherwise = p1 - p2 
                     

diatonicInterval, chromaticInterval :: Interval -> Bool
diatonicInterval _ = undefined -- perfect major or minor

chromaticInterval = not . diatonicInterval

intervalClass :: Interval -> Bool
intervalClass = undefined

         




 

                