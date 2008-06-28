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
  Interval, NamedInterval(..), IntervalQuality(..), IntervalPattern(..), 
  
  -- * smart constructor, destructor and selectors
  interval, unInterval, intervalType, halfSteps,
  
  -- * Typeclasses
  IntervalExtension(..),
  
  -- * operations
  interval', buildInterval, arithmeticDistance


    
  ) where

import Bala.Base.Pitch
import Bala.Base.PitchClass
import Bala.Base.BaseExtra 

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------


-- | Represent intervals with a pair of integers. 
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


-- | Smart constructor for Intervals.
interval :: Int -> Int -> Interval
interval d s 
    | d > 0 && s >= 0 = Interval (Count d) (Count s)
    | d == 0          = error dzero_msg
    | otherwise       = error mixed_msg
 where
  dzero_msg = "Cannot create an Interval with an interval type of 0"
  mixed_msg = "Cannot create an Interval with negative numbers for\n"
           ++ "interval type or semitone count"  

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


           
-- | A named interval 
data NamedInterval = NamedInterval {
    interval_measure :: IntervalMeasure,
    interval_quality :: IntervalQuality,
    interval_size    :: IntervalSize
  }
  deriving (Eq,Show)  
  


-- | Commonly named interval sizes.  
data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth
                  | Seventh | Octave
  deriving (Eq,Enum,Ord,Show,Read)

-- | Commonly named interval qualities.  
data IntervalQuality = Perfect | Major | Minor | Augmented | Diminished
                     | DoublyAugmented | DoublyDiminished
  deriving (Eq,Enum,Ord,Show,Read)

-- | Length of the arithmetic distance 
data IntervalMeasure = Simple | Compound Int
  deriving (Eq,Ord,Show)

newtype IntervalPattern = IntervalPattern { unIntervalPattern :: [Interval] }
  deriving (Show)


--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

-- | Extend a \pitched value\ by an 'Interval'.
class IntervalExtension a where
  -- | Increase a \pitched value\ by an 'Interval'.
  extUp   :: a -> Interval -> a
  -- | Decrease a \pitched value\ by an 'Interval'.
  extDown :: a -> Interval -> a 

  
  
  

   
        
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------



interval' :: IntervalQuality -> Int -> Interval
interval' Perfect           = iperfect    . toEnum . (flip (-) 1)
interval' Major             = imajor      . toEnum . (flip (-) 1)
interval' Minor             = iminor      . toEnum . (flip (-) 1)
interval' Augmented         = iaugmented  . toEnum . (flip (-) 1)
interval' Diminished        = idiminished . toEnum . (flip (-) 1)
interval' DoublyAugmented   = undefined
interval' DoublyDiminished  = undefined



    
--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------
  
  
  
-- | fromSemis is biased, it produces m6 rather than A5 & A4 and A4 
-- rather than d4.
fromSemis :: (Integral a) => a -> Interval
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


intervalName :: Interval -> Maybe NamedInterval
intervalName invl = 
    let (measure, simple) = simplify invl in fn measure (unInterval simple)
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

-- 
half_step_m2  = interval 2 1
whole_step_M2 = interval 2 2 

-- | Build a perfect interval from a size.
iperfect :: IntervalSize -> Interval
iperfect Unison   = interval 1 0
iperfect Fourth   = interval 4 5
iperfect Fifth    = interval 5 7
iperfect Octave   = interval 8 12
iperfect z        = error $ 
    "cannot build a major interval for " ++ show z
    
-- | Build a major interval from a size.
imajor :: IntervalSize -> Interval
imajor Second   = interval 2 2
imajor Third    = interval 3 4
imajor Sixth    = interval 6 9
imajor Seventh  = interval 7 11
imajor z        = error $ 
    "cannot build a major interval for " ++ show z
    
-- | Build a minor interval from a size.
iminor :: IntervalSize -> Interval
iminor Second   = interval 2 1
iminor Third    = interval 3 3
iminor Sixth    = interval 6 8
iminor Seventh  = interval 7 9
iminor z        = error $ 
    "cannot build a minor interval for " ++ show z
    
-- | Build an augmented interval from a size.
iaugmented :: IntervalSize -> Interval
iaugmented Unison   = interval 1 1
iaugmented Second   = interval 2 3
iaugmented Third    = interval 3 4
iaugmented Fourth   = interval 4 6
iaugmented Fifth    = interval 5 8
iaugmented Sixth    = interval 6 10
iaugmented z        = error $ 
    "cannot build an augmented interval for " ++ show z

-- | Build a diminished interval from a size.
idiminished :: IntervalSize -> Interval
idiminished Second    = interval 2 1  -- aka minor second
idiminished Third     = interval 3 2
idiminished Fourth    = interval 4 4
idiminished Fifth     = interval 5 6
idiminished Sixth     = interval 6 7
idiminished Seventh   = interval 7 9
idiminished Octave    = interval 8 11
idiminished z = error $ 
    "cannot build an diminished interval for " ++ show z



intervalList :: IntervalPattern -> [Interval]
intervalList s = let ivals = unIntervalPattern s in
  scanl extUp (interval 1 0) ivals
  
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
-- Instances
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
           

  
  
instance Enum IntervalMeasure where
  fromEnum Simple       = 0 
  fromEnum (Compound i) = i 
  

  toEnum i | i == 0    = Simple
           | i >  0    = Compound i
           | otherwise = Compound (abs i)
           
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
  affi = (maybe id affi) . intervalName
  
instance Affi NamedInterval where
  affi (NamedInterval m q s) = affi m . affi q . showChar '-' . affi s 
    
    
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

instance Affi IntervalMeasure where
  affi Simple       = id
  affi (Compound i) = shows i . showChar 'x'

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
  deco = decoInterval

-- | Parsec parser for 'Interval'. Needs more work...
decoInterval :: Parser Interval 
decoInterval = (maybe err id . namedInterval) <$> decoNamedInterval
  where err = error $ "could not build an interval"
  
instance Deco NamedInterval where
  deco = decoNamedInterval

-- | Parsec parser for 'NamedInterval'.
decoNamedInterval :: Parser NamedInterval  
decoNamedInterval = NamedInterval <$> decoIntervalMeasure 
                                  <*> decoIntervalQuality 
                                  <*> (lexString "-" *> decoIntervalSize)

    
instance Deco IntervalMeasure where
  deco = decoIntervalMeasure

-- | Parsec parser for 'IntervalMeasure'.
decoIntervalMeasure :: Parser IntervalMeasure  
decoIntervalMeasure = option Simple (Compound <$> (int <* lexString "x")) 
  
  
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
    aug   = iaugmented    <$> (lexChar 'A' *> intIntervalSize)
    majr  = imajor        <$> (lexChar 'M' *> intIntervalSize)        
    minr  = iminor        <$> (lexChar 'm' *> intIntervalSize)
    perf  = iperfect      <$> (lexChar 'P' *> intIntervalSize)
    dim   = idiminished   <$> (lexChar 'd' *> intIntervalSize)
    
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
                  
                  




 

                