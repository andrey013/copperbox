{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

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
--
--------------------------------------------------------------------------------



module Bala.Base.Interval (
  -- * Datatypes (Interval is opaque) 
  Interval, NamedInterval, IntervalSize(..), IntervalQuality(..), 
  IntervalPattern(..), 
  
  -- * Construct and deconstruct intervals
  interval, unInterval, intervalType, halfSteps,
  intervalSize,
  
  -- * Typeclasses
  IntervalExtension(..),
  
  -- * operations
  perfectInterval, majorInterval, minorInterval, augmentedInterval, 
  diminishedInterval,
  
  pitchDifference, arithmeticDistance,
  
  
  -- * Named elements
  perfect_unison, perfect_fourth, perfect_fifth, perfect_octave,
  major_second, major_third, major_sixth, major_seventh,
  minor_second, minor_third, minor_sixth, minor_seventh,
  diminished_third, diminished_fifth,
  augmented_second, augmented_third, augmented_fourth, augmented_fifth

    
  ) where

import Bala.Base.Pitch
import Bala.Base.PitchClass
import Bala.Base.BaseExtra 

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------


-- | Represent intervals as a pair of integers.
-- To make interval addition and especially subtraction easier, the integers
-- are wrapped by 'Count'. Subtraction does not produce negative intervals! 
data Interval = Interval { 
    -- | Count of pitch letters, it is /retrograde/ on the first note.
    -- i.e. C to C - a unison - counts 1 (rather than 0), 
    --      C to D - a second - counts 2, etc.
    interval_type :: Count RNnNz,
    -- | The number of half steps between pitched values. 
    half_steps :: Count NonNeg 
  }
  deriving (Eq,Show)

           
-- | Commonly named interval qualities.  
data IntervalQuality = Perfect | Major | Minor | Augmented | Diminished
                     | DoublyAugmented | DoublyDiminished
  deriving (Eq,Enum,Ord,Show,Read)
  
             
-- | A named interval 
data NamedInterval = NamedInterval {
    -- | Zero is a simple interval, 1 - one octave compound, 
    -- 2 - two octave compound, etc.
    interval_measure :: Int,
    interval_quality :: IntervalQuality,
    interval_size    :: IntervalSize
  }
  deriving (Eq,Show)  
  


-- | Commonly named interval sizes.  
data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth
                  | Seventh | Octave
  deriving (Eq,Enum,Ord,Show,Read)


newtype IntervalPattern = IntervalPattern { unIntervalPattern :: [Interval] }
  deriving (Show)

--------------------------------------------------------------------------------
-- Constructors and selectors

interval :: IntervalQuality -> IntervalSize -> Interval
interval Perfect           = perfectInterval
interval Major             = majorInterval
interval Minor             = minorInterval
interval Augmented         = augmentedInterval
interval Diminished        = diminishedInterval
interval DoublyAugmented   = error "interval DoublyAugmented" 
interval DoublyDiminished  = error "interval DoublyDiminished"


buildInterval :: Int -> Int -> Interval
buildInterval d s = Interval (Count d) (Count s) 



-- | Destructor for intervals.           
unInterval :: Interval -> (Int,Int)
unInterval (Interval (Count ad) (Count hs)) = (ad,hs)

-- | Selector for interval type (unison, second, etc. - the pitch letter count) 
-- of an @Interval@.
intervalType :: Interval -> Int
intervalType (Interval (Count d) _) = d

-- | Selector for @half_steps@ (semitone count) of an @Interval@.
halfSteps :: Interval -> Int
halfSteps (Interval _ (Count s)) = s


namedInterval :: Interval -> Maybe NamedInterval
namedInterval ivl = 
    let (oc, simple) = reduceInterval ivl in fn oc (unInterval simple)
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
    
intervalSize :: Int -> IntervalSize    
intervalSize i | i > 0 = toEnum $ sub1 i 


--------------------------------------------------------------------------------
-- Type classes

-- | Extend a \pitched value\ by an 'Interval'.
class IntervalExtension a where
  -- | Increase a \pitched value\ by an 'Interval'.
  extUp   :: a -> Interval -> a
  -- | Decrease a \pitched value\ by an 'Interval'.
  extDown :: a -> Interval -> a 

  
  
  

   
        





    
--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Build a perfect interval from a size.
perfectInterval :: IntervalSize -> Interval
perfectInterval Unison   = buildInterval 1 0
perfectInterval Fourth   = buildInterval 4 5
perfectInterval Fifth    = buildInterval 5 7
perfectInterval Octave   = buildInterval 8 12
perfectInterval z        = error $ 
    "cannot build a major interval for " ++ show z
    
-- | Build a major interval from a size.
majorInterval :: IntervalSize -> Interval
majorInterval Second   = buildInterval 2 2
majorInterval Third    = buildInterval 3 4
majorInterval Sixth    = buildInterval 6 9
majorInterval Seventh  = buildInterval 7 11
majorInterval z        = error $ 
    "cannot build a major interval for " ++ show z
    
-- | Build a minor interval from a size.
minorInterval :: IntervalSize -> Interval
minorInterval Second   = buildInterval 2 1
minorInterval Third    = buildInterval 3 3
minorInterval Sixth    = buildInterval 6 8
minorInterval Seventh  = buildInterval 7 9
minorInterval z        = error $ 
    "cannot build a minor interval for " ++ show z
    
-- | Build an augmented interval from a size.
augmentedInterval :: IntervalSize -> Interval
augmentedInterval Unison   = buildInterval 1 1
augmentedInterval Second   = buildInterval 2 3
augmentedInterval Third    = buildInterval 3 4
augmentedInterval Fourth   = buildInterval 4 6
augmentedInterval Fifth    = buildInterval 5 8
augmentedInterval Sixth    = buildInterval 6 10
augmentedInterval z        = error $ 
    "cannot build an augmented interval for " ++ show z

-- | Build a diminished interval from a size.
diminishedInterval :: IntervalSize -> Interval
diminishedInterval Second    = buildInterval 2 1  -- aka minor second
diminishedInterval Third     = buildInterval 3 2
diminishedInterval Fourth    = buildInterval 4 4
diminishedInterval Fifth     = buildInterval 5 6
diminishedInterval Sixth     = buildInterval 6 7
diminishedInterval Seventh   = buildInterval 7 9
diminishedInterval Octave    = buildInterval 8 11
diminishedInterval z = error $ 
    "cannot build an diminished interval for " ++ show z
    

-- | Is it a compound interval? 
compound :: Interval -> Bool
compound ivl = 12 < halfSteps ivl

-- | Is it a simple interval? 
simple :: Interval -> Bool
simple ivl = 12 >= halfSteps ivl


reduceInterval :: Interval -> (Int,Interval)
reduceInterval ivl = rec 0 ivl
  where  
    rec i ivl | simple ivl  = (i,ivl)
              | otherwise   = rec (i+1) (ivl - p8) 
    p8 = interval Perfect Octave 

          
  
-- | fromSemis is biased, it produces m6 rather than A5 & A4 and A4 
-- rather than d4.
fromSemis :: (Integral a) => a -> Interval
fromSemis i = let i' = fromIntegral (abs i) in buildInterval (stepy i') i'  

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
  
    
-- | A /retrograde/ count of pitch letters, C to C is 1, C to D is 2, etc.
letterCountTo :: PitchLetter -> PitchLetter -> Int
letterCountTo = retroCountTo succ


-- | number of 'letter names' inclusive between the lowest and highest pitch
arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance p  p' | p < p'    = aCount p  p'
                         | otherwise = aCount p' p
  where
    aCount p p' = let o   = octaveMeasure p
                      o'  = octaveMeasure p'
                      l   = pitchLetter p
                      l'  = pitchLetter p'
                  in retroCountTo succ (o,l) (o',l')
--                   
   

arithmeticStep :: Pitch -> Int -> Pitch                                
arithmeticStep pch i = 
  let name        = pitchLetter pch
      o           = octaveMeasure pch
      (o',name')  = applyi succ (o,name) (i - 1)
  in pitch (PitchName name' Nat) o
   


pitchDifference :: Pitch -> Pitch -> Interval
pitchDifference p  p' = 
  buildInterval (p `arithmeticDistance` p') (p `semitoneDistance` p')


invert :: Interval -> Interval
invert (Interval (Count d) (Count s)) 
  | d < 9     = buildInterval (9-d) (12-s)
  | otherwise = error "todo invert on large intervals ..."



-- 
half_step_m2  = buildInterval 2 1
whole_step_M2 = buildInterval 2 2 





intervalList :: IntervalPattern -> [Interval]
intervalList s = let ivals = unIntervalPattern s in
  scanl extUp (buildInterval 1 0) ivals
  
--------------------------------------------------------------------------------
-- old.... 



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

--------------------------------------------------------------------------------
-- | Named elements


perfect_unison, perfect_fourth, perfect_fifth, perfect_octave :: Interval
perfect_unison    = perfectInterval Unison
perfect_fourth    = perfectInterval Fourth
perfect_fifth     = perfectInterval Fifth
perfect_octave    = perfectInterval Octave

major_second, major_third, major_sixth, major_seventh :: Interval
major_second      = majorInterval Second
major_third       = majorInterval Third
major_sixth       = majorInterval Sixth
major_seventh     = majorInterval Seventh

minor_second, minor_third, minor_sixth, minor_seventh :: Interval
minor_second      = minorInterval Second
minor_third       = minorInterval Third
minor_sixth       = minorInterval Sixth
minor_seventh     = minorInterval Seventh

diminished_third, diminished_fifth :: Interval
diminished_third  = diminishedInterval Third
diminished_fifth  = diminishedInterval Fifth

augmented_second, augmented_third, augmented_fourth, augmented_fifth 
    :: Interval
augmented_second  = augmentedInterval Second
augmented_third   = augmentedInterval Third
augmented_fourth  = augmentedInterval Fourth
augmented_fifth   = augmentedInterval Fifth




--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Num Interval where
  
  (Interval d s) + (Interval d' s') = 
    Interval (d `countPlus` d') (s `countPlus` s')

  (Interval d s) - (Interval d' s') =  
      Interval (d `countMinus` d') (s `countMinus` s')
  
  (Interval (Count d) (Count s)) * (Interval (Count d') (Count s')) =  
      buildInterval (d * d') (s * s')
    
  abs  = id
  
  negate _ = buildInterval 1 0
  
  signum (Interval d s) = buildInterval 1 0
  
  fromInteger = fromSemis      
           

           
instance Ord Interval where
  (Interval _ s) `compare` (Interval _ s') = s `compare` s'


instance Enum (Int,PitchLetter) where
  succ (o,B) = (o+1,C)
  succ (o,l) = (o,succ l)
  
  pred (o,C) = (o-1,B)
  pred (o,l) = (o, pred l)
  
  fromEnum (o,l) = o * 8 + fromEnum l
  toEnum i = let (o,il) = i `divMod` 8 in (o, toEnum il)
  

instance Semitones Interval where
  semitoneCount (Interval _ (Count sc)) = sc

{-    
instance SemitoneExtension Interval where 
  addSemi (Interval d s) i = Interval d (s `forward` i)
  subSemi (Interval d s) i = Interval d (s `backward` i)
  
-}

instance IntervalExtension Interval where
  extUp   = (+)
  extDown = (-)
  
instance IntervalExtension PitchName where
  extUp lbl@(PitchName l _) (Interval ad sc) = 
    let l' = successor l (unCount ad - 1)
    in spell (lbl `addSemi` (unCount sc)) l'
    
  extDown lbl@(PitchName l _) (Interval ad sc) = 
    let l' = predecessor l (unCount ad - 1)
    in spell (lbl `subSemi` (unCount sc)) l'


-- | {SPELLING ? }  

instance IntervalExtension Pitch where
  extUp pch inval =
    let lbl = extUp (pitchName pch) inval
        (o,s,c) = pitchMeasures pch
        sc      = halfSteps inval
        (oc,_)  = explode12 $ s + sc       
    in pitch lbl (o + oc) `withCents` c
    
  extDown pch inval =
    let lbl = extDown (pitchName pch) inval
        (o,s,c) = pitchMeasures pch 
        sc      = halfSteps inval
        (oc,_) = explode12 $ s - sc      
    in pitch lbl (o - oc) `withCents` c
    
      
--------------------------------------------------------------------------------
-- Affi instances
--------------------------------------------------------------------------------

instance Affi Interval where
  affi = (maybe id affi) . namedInterval
  
instance Affi NamedInterval where
  affi (NamedInterval m q s) = affiIntervalMeasure m . affi q . showChar '-' . affi s 
    
    
instance Affi IntervalSize where
  affi Unison   = showString "unison" 
  affi Second   = showString "second" 
  affi Third    = showString "third" 
  affi Fourth   = showString "fourth" 
  affi Fifth    = showString "fifth" 
  affi Sixth    = showString "sixth" 
  affi Seventh  = showString "seventh" 
  affi Octave   = showString "octave"


instance Affi IntervalQuality where
  affi Perfect          = showString "perfect"
  affi Major            = showString "major"
  affi Minor            = showString "minor"
  affi Augmented        = showString "augmented"
  affi Diminished       = showString "diminished"
  affi DoublyAugmented  = showString "doubly-augmented"
  affi DoublyDiminished = showString "doubly-diminished"

affiIntervalMeasure i | i == 0    = id
                      | otherwise = shows i . showChar 'x'

instance Affi IntervalPattern where
  affi (IntervalPattern xs) = hsepS $ map (step . halfSteps) xs
    where
      step 1 = showChar 'H'
      step 2 = showChar 'W'
      step 3 = showString "A2"  
      
      
--------------------------------------------------------------------------------
-- Deco instances
--------------------------------------------------------------------------------

instance Deco Interval where
  deco = undefined -- decoInterval

{-
-- | Parsec parser for 'Interval'. Needs more work...
decoInterval :: Parser Interval 
decoInterval = (maybe err id . namedInterval) <$> decoNamedInterval
  where err = error $ "could not build an interval"
-}

  
instance Deco NamedInterval where
  deco = decoNamedInterval

-- | Parsec parser for 'NamedInterval'.
decoNamedInterval :: Parser NamedInterval  
decoNamedInterval = NamedInterval <$> decoIntervalMeasure 
                                  <*> decoIntervalQuality 
                                  <*> (lexString "-" *> decoIntervalSize)

    

-- | Parsec parser for 'IntervalMeasure'.
decoIntervalMeasure :: Parser Int  
decoIntervalMeasure = option 0 (int <* lexString "x")
  
  
instance Deco IntervalQuality where
  deco = decoIntervalQuality

-- | Parsec parser for 'IntervalQuality'.
decoIntervalQuality :: Parser IntervalQuality
decoIntervalQuality = 
    choice [perfect,major,minor,augmented,diminished,dblaug,dbldim]
  where
    perfect     = Perfect          <$ lexString "perfect"
    major       = Major            <$ lexString "major"
    minor       = Minor            <$ lexString "minor"
    augmented   = Augmented        <$ lexString "augmented"
    diminished  = Diminished       <$ lexString "diminished"
    dblaug      = DoublyAugmented  <$ lexString "doubly-diminished" 
    dbldim      = DoublyDiminished <$ lexString "doubly-diminished"
          
instance Deco IntervalSize where
  deco = decoIntervalSize

-- | Parsec parser for 'IntervalSize'.
decoIntervalSize :: Parser IntervalSize
decoIntervalSize = 
    choice [unison,second,third,fourth,fifth,sixth,seventh,octave]
  where 
    unison      = Unison  <$ lexString "unison" 
    second      = Second  <$ lexString "second" 
    third       = Third   <$ lexString "third" 
    fourth      = Fourth  <$ lexString "fourth" 
    fifth       = Fifth   <$ lexString "fifth" 
    sixth       = Sixth   <$ lexString "sixth" 
    seventh     = Seventh <$ lexString "seventh" 
    octave      = Octave  <$ lexString "octave"

        
instance Deco IntervalPattern where 
  deco = decoIntervalPattern

decoIntervalPattern :: Parser IntervalPattern  
decoIntervalPattern  = IntervalPattern <$> many1 shortfromInterval

shortfromInterval :: Parser Interval
shortfromInterval = choice [whole,half,aug,majr,minr,perf,dim]
  where 
    whole = whole_step_M2 <$  lexChar 'W'
    half  = half_step_m2  <$  lexChar 'H'
    aug   = augmentedInterval    <$> (lexChar 'A' *> intIntervalSize)
    majr  = majorInterval        <$> (lexChar 'M' *> intIntervalSize)        
    minr  = minorInterval        <$> (lexChar 'm' *> intIntervalSize)
    perf  = perfectInterval      <$> (lexChar 'P' *> intIntervalSize)
    dim   = diminishedInterval   <$> (lexChar 'd' *> intIntervalSize)
    
intIntervalSize :: Parser IntervalSize
intIntervalSize = 
    choice [unison,second,third,fourth,fifth,sixth,seventh,octave]
  where 
    unison      = Unison  <$ lexChar '1' 
    second      = Second  <$ lexChar '2' 
    third       = Third   <$ lexChar '3'
    fourth      = Fourth  <$ lexChar '4'
    fifth       = Fifth   <$ lexChar '5'
    sixth       = Sixth   <$ lexChar '6'
    seventh     = Seventh <$ lexChar '7'
    octave      = Octave  <$ lexChar '8'
                  
                  




 

                