{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  UTT.Neume
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Neume
--
--------------------------------------------------------------------------------

module UTT.Neume
  

   where

import UTT.Base
import UTT.Z12

import Neume.Core.Pitch
import Neume.Core.SpellingMap

data Pitch3 = P3 Pitch Pitch Pitch
  deriving (Eq,Show)

-- This representation is not very good - it would
-- be much better to have arithmetic...

upInt :: [Int] -> [Int]
upInt = mapWithPrevAns up
  where
    up p n | n < p      = (p+1)
           | otherwise  = n


upwards :: [Pitch3] -> [Pitch3]
upwards = mapWithPrevAns up where
  up (P3 r1 t1 f1) current@(P3 r2 t2 f2) 
    | r2 < r1   = P3 (r2 `raiseAbove` r1) (t2 `raiseAbove` t1) (f2 `raiseAbove` f1)
    | otherwise = current

raiseAbove :: Pitch -> Pitch -> Pitch
p1 `raiseAbove` p2 | p1 >= p2 = p1
                   | otherwise = (addOve p1) `raiseAbove` p2
  where
    addOve (Pitch l a ove) = Pitch l a (ove+1)

mapWithPrevAns :: (a -> a -> a) -> [a] -> [a]
mapWithPrevAns f (x:xs) = x : step x xs 
  where
    step _ []     = []
    step p (a:as) = let ans = f p a in ans : step ans as

mapWithPrev _ xs       = xs   -- catch zero or one


pitches :: SpellingMap -> Triad -> Pitch3
pitches spellings (Triad root mode) = step mode where
  step Pos = P3 pch (addMajorThird pch) (addPerfectFifth pch)
  step Neg = P3 pch (addMinorThird pch) (addPerfectFifth pch)

  (PitchLabel l a) = pitchLabel root
  pch              = spell spellings (Pitch l a 4 )


pitchLabel :: Z12 -> PitchLabel
pitchLabel  0 = PitchLabel C Nothing
pitchLabel  1 = PitchLabel C (Just Sharp)
pitchLabel  2 = PitchLabel D Nothing
pitchLabel  3 = PitchLabel D (Just Sharp)
pitchLabel  4 = PitchLabel E Nothing
pitchLabel  5 = PitchLabel F Nothing
pitchLabel  6 = PitchLabel C (Just Sharp)
pitchLabel  7 = PitchLabel G Nothing
pitchLabel  8 = PitchLabel G (Just Sharp)
pitchLabel  9 = PitchLabel A Nothing
pitchLabel 10 = PitchLabel A (Just Sharp)
pitchLabel  _ = PitchLabel B Nothing

countup :: PitchLetter -> Int -> PitchLetter
countup p i = toEnum $ mod7 $ i + fromEnum p
  where
    mod7 n = n `mod` 7 

addMajorThird       :: Pitch -> Pitch
addMajorThird pch   = pch `addInterval` (3,4)

addMinorThird       :: Pitch -> Pitch
addMinorThird pch   = pch `addInterval` (3,3)

addPerfectFifth     :: Pitch -> Pitch
addPerfectFifth pch = pch `addInterval` (5,7)

type Interval = (Int,Int)

-- ad - 1
addInterval :: Pitch -> Interval -> Pitch
addInterval pch@(Pitch lbl _ ove) (ad,sc) = Pitch lbl' (fn acc') ove'
  where
    lbl'    = lbl `countup` (ad-1)
    ove'    = if (lbl' < lbl) then (ove+1) else ove
    scdif   = semitoneCount (Pitch lbl' Nothing ove') - semitoneCount pch
    acc'    = if scdif > sc then countdownAccidental Nat (scdif - sc)
                            else countupAccidental   Nat (sc - scdif) 
    fn Nat  = Nothing
    fn a    = Just a


moveAccidental :: Accidental -> Int -> Accidental
moveAccidental a i | i < 0     = countdownAccidental a (abs i)
                   | otherwise = countupAccidental a i

-- | Note - upper bounded at DoubleSharp
--
countupAccidental :: Accidental -> Int -> Accidental
countupAccidental a           i | i <= 0 = a
countupAccidental DoubleSharp i | i >  0 = DoubleSharp
countupAccidental a           i          = countupAccidental (succ a) (i-1)


-- | Note - upper bounded at DoubleSharp
--
countdownAccidental :: Accidental -> Int -> Accidental
countdownAccidental a          i | i <= 0 = a
countdownAccidental DoubleFlat i | i >  0 = DoubleFlat
countdownAccidental a          i          = countdownAccidental (pred a) (i-1)