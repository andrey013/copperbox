{-# OPTIONS_GHC -XFlexibleInstances #-}

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
    arith_dist :: Count RNnNz, 
    half_steps :: Count NonNeg 
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
    Interval (d `countPlus` d') (s `countPlus` s')

  (Interval d s) - (Interval d' s') =  
      Interval (d `countMinus` d') (s `countMinus` s')
  
  (Interval (Count d) (Count s)) * (Interval (Count d') (Count s')) =  
      interval (d * d') (s * s')
    
  abs  = id
  
  negate _ = interval 1 0
  
  signum (Interval d s) = interval 1 0
  
  fromInteger = fromSemis      
           
instance Semitones Interval where
  semitones (Interval _ (Count sc)) = sc
    
instance SemiDisplacement Interval where 
  addSemi (Interval d s) i = Interval d (s `forward` i)
  subSemi (Interval d s) i = Interval d (s `backward` i)
  
  
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

-- Smart constructor for Intervals
-- TODO - Intervals be can constructed if the arithmetic distance and the 
-- semitone count don't make sense 
-- eg (1,8) should only have (1,1) or (1,0)
-- (unless of course wild pitch spelling is considered)

interval d s 
    | d > 0 && s >= 0 = Interval (Count d) (Count s)
    | d == 0          = error dzero_msg
    | otherwise       = error mixed_msg
 where
  dzero_msg = "Cannot create an Interval with an arithmetic distance of 0"
  mixed_msg = "Cannot create an Interval with negative numbers for\n"
           ++ "arithmetic distance or semitone count"  
  
--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- fromSemis is biased 
-- it produces m6 rather than A5 & A4 and A4 rather than d4
fromSemis i = let i' = fromIntegral (abs i) in interval (stepy i') i'  

stepy :: (Num a, Ord a) => a -> a
stepy i | i < 0       = stp $ abs i
        | otherwise   = stp i
  where
    stp i | i == 0                       = 1
          | i == 1  || i == 2            = 2
          | i == 3  || i == 4 || i == 5  = 3
          | i == 6                       = 4
          | i == 7                       = 5
          | i == 8  || i == 9            = 6
          | i == 10 || i == 11           = 7
          | i == 12                      = 8
          | otherwise                    = 7 + stp (i - 13)
  
    

--------------------------------------------------------------------------------
-- Ord Instance
--------------------------------------------------------------------------------

instance Ord Interval where
  (Interval _ s) `compare` (Interval _ s') = s `compare` s'
  

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

retroCount :: (Eq a) => (a -> a) -> a -> a -> Int
retroCount fn x y = snd $ until p f (x,1)
  where 
    p (a,_) = a == y
    f (a,i) | i > 1000  = error "retrocount - recursion limit 1000"
            | otherwise = (fn a, i+1)
 


-- 'retrograde' C-C is 1, C-D is 2 ..
letterCount :: PitchLetter -> PitchLetter -> Int
letterCount = retroCount succ


instance Enum (Int,PitchLetter) where
  succ (o,B) = (o+1,C)
  succ (o,l) = (o,succ l)
  
  pred (o,C) = (o-1,B)
  pred (o,l) = (o, pred l)
  
  fromEnum (o,l) = o * 8 + fromEnum l
  toEnum i = let (o,il) = i `divMod` 8 in (o, toEnum il)

-- | number of 'letter names' inclusive between the lowest and highest pitch
arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance p  p' | p < p'    = aCount p  p'
                         | otherwise = aCount p' p
  where
    aCount (Pitch (PitchLabel l _) o _ _) (Pitch (PitchLabel l' _) o' _ _) = 
      retroCount succ (o,l) (o',l')
   
    

arithmeticStep :: Pitch -> Int -> Pitch                                
arithmeticStep (Pitch (PitchLabel l _) o _ _) i = 
    buildPitch (PitchLabel l' Nat) o' 0
  where
    (o',l') = applyi succ (o,l) (i - 1)



buildInterval :: Pitch -> Pitch -> Interval
buildInterval p  p' = 
  interval (p `arithmeticDistance` p') (p `semitoneDistance` p')


invert :: Interval -> Interval
invert (Interval (Count d) (Count s)) 
  | d < 9     = interval (9-d) (12-s)
  | otherwise = error "todo invert on large intervals ..."
 
simplify :: Interval -> (IntervalMeasure,Interval)
simplify (Interval (Count d) (Count s)) 
  | d > 8 && s > 12 = let (i,s') = s `divMod` 12 
                      in (Compound i, interval (d `mod` 8) s')
  | otherwise       = (Simple, interval d s) 


unWrapInterval (Interval (Count d) (Count s)) = (d,s) 

intervalName :: Interval -> Maybe NamedInterval
intervalName invl = 
    let (measure, simple) = simplify invl in fn measure (unWrapInterval simple)
  where
    fn m (1,0)   = Just (NamedInterval m Perfect Unison)   
    fn m (2,1)   = Just (NamedInterval m Minor Second)
    fn m (2,2)   = Just (NamedInterval m Major Second)
    fn m (3,3)   = Just (NamedInterval m Minor Third)
    fn m (3,4)   = Just (NamedInterval m Major Third)
    fn m (4,5)   = Just (NamedInterval m Perfect Fourth)
    fn m (4,6)   = Just (NamedInterval m Augmented Fourth)
    fn m (5,6)   = Just (NamedInterval m Diminished Fifth)
    fn m (5,7)   = Just (NamedInterval m Perfect Fifth)
    fn m (5,8)   = Just (NamedInterval m Augmented Fifth)
    fn m (6,8)   = Just (NamedInterval m Minor Sixth)
    fn m (6,9)   = Just (NamedInterval m Major Sixth)
    fn m (7,10)  = Just (NamedInterval m Minor Seventh)
    fn m (7,11)  = Just (NamedInterval m Major Seventh)
    fn m (8,12)  = Just (NamedInterval m Perfect Octave)
    fn m _       = Nothing



namedInterval :: NamedInterval -> Maybe Interval
namedInterval (NamedInterval msr qlty sz) = fmap (mk msr) (fn qlty sz)
  where 
    mk Simple       (a,s) = interval a s
    mk (Compound i) (a,s) = interval (a + 8 * i) (s + 12 * i)

    fn Perfect    Unison  = Just (1,0)
    fn Minor      Second  = Just (2,1)
    fn Major      Second  = Just (2,2)
    fn Minor      Third   = Just (3,3)
    fn Major      Third   = Just (3,4)
    fn Perfect    Fourth  = Just (4,5) 
    fn Augmented  Fourth  = Just (4,6)
    fn Diminished Fifth   = Just (5,6)
    fn Perfect    Fifth   = Just (5,7)
    fn Augmented  Fifth   = Just (5,8)
    fn Minor      Sixth   = Just (6,8)
    fn Major      Sixth   = Just (6,9)
    fn Minor      Seventh = Just (7,10)
    fn Major      Seventh = Just (7,11)
    fn Perfect    Octave  = Just (8,12)
    fn  _         _       = Nothing 


extr :: Pitch -> Interval -> Pitch
extr p@(Pitch (PitchLabel l a) o s c) inval =
    let (ad,sc) = unWrapInterval inval
        (oc,s') = explode12 $ s + sc
        l'      = successor l (ad - 1)        
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

         




 

                