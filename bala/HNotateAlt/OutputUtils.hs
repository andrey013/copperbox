
--------------------------------------------------------------------------------
-- |
-- Module      :  OutputUtils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Output Pitch and Duration as LilyPond, Abc ...
--
--------------------------------------------------------------------------------


-- Do the conversions properly in this module...

module OutputUtils where

import CommonUtils
import Duration
import Pitch
import qualified TextAbc as Abc 
import qualified TextLilyPond as Ly 

import Data.Ratio
import Data.Word

--------------------------------------------------------------------------------
-- Abc


-- todo - check what Abc does with regards to accidentals and key signatures 
abcNote :: Pitch -> Abc.AbcNote
abcNote (Pitch l a o) = 
    let pl = if o > 4 then upwards $ abcPitchLetter l else abcPitchLetter l
    in (oabcAccidental a !*> Abc.note pl *! oabcOve o)


abcPitchLetter :: PitchLetter -> Abc.AbcPitchLetter
abcPitchLetter = toEnum . fromEnum

upwards :: Abc.AbcPitchLetter -> Abc.AbcPitchLetter
upwards l = let i = fromEnum l in 
  if (i<7) then toEnum $ i + 7 else l


oabcAccidental :: Accidental -> Maybe Abc.AbcAccidental
oabcAccidental Nat          = Nothing
oabcAccidental a            = Just $ abcAccidental a

abcAccidental :: Accidental -> Abc.AbcAccidental
abcAccidental Nat            = Abc.natural
abcAccidental Sharp          = Abc.sharp
abcAccidental Flat           = Abc.flat
abcAccidental DoubleSharp    = Abc.doubleSharp
abcAccidental DoubleFlat     = Abc.doubleFlat

oabcOve :: Int -> Maybe Abc.AbcOctave    
oabcOve i | i < 4       = Just $ Abc.octaveLow (4-i)
          | i > 5       = Just $ Abc.octaveHigh (i-5) 
          | otherwise   = Nothing
          
          
abcRelativeDuration :: Duration -> Duration -> Maybe Abc.AbcDuration
abcRelativeDuration base drn@(Duration _ dots)
    | base == drn   = Nothing 
    | otherwise     = case scaler base drn of
                        Left i -> Just $ Abc.dmult (doti i dots)
                        Right r -> Just $ factor $ dotr r dots
  where
    factor r = let (n,d) = (numerator r, denominator r)
               in if n==1 then Abc.ddiv1 d else Abc.ddiv2 (n,d)                     

-- if the duration is longer it is scaled by an int, 
-- if it is shorter its scaled by a fraction. 
scaler :: Duration -> Duration -> Either Int (Ratio Int)
scaler (Duration base _) (Duration drn _) = 
    let a = drn / base; n = numerator a; d = denominator a
    in if d == 1 then Left n else Right a
    
    
doti i dots = sum $ map (i `div`) xs
  where xs = take (dots+1) base2number_sequence

dotr r dots = sum $ map ((r /) . fromIntegral) xs
  where xs = take (dots+1) base2number_sequence  
    
--------------------------------------------------------------------------------
-- LilyPond

-- 'lypitch' produces an absolute pitch 
-- (i.e. the octave spec is not relative to the previous note). 
lypitch :: Pitch -> Ly.LyPitch
lypitch p@(Pitch l a o) = 
    let mp = lymodpitch p
        oo = olyOctaveSpec (o-4)
    in mp *! oo

-- 'lymodpitch' produces a modulus pitch (i.e. has no octave spec) 
lymodpitch :: Pitch -> Ly.LyPitch
lymodpitch (Pitch l a _) = 
    let pn = lyPitchName l
        oa = olyAccidental a 
    in Ly.pitch pn *! oa


lyRelativeOctave :: Pitch -> Pitch -> Maybe Ly.LyOctaveSpec
lyRelativeOctave base pch = let dist = base `octaveDist` pch in
    case dist `compare` 0 of
      LT -> Just $ Ly.lowered $ abs dist
      GT -> Just $ Ly.raised dist
      EQ -> Nothing


lyPitchName :: PitchLetter -> Ly.LyPitchName
lyPitchName = toEnum . fromEnum 

olyAccidental :: Accidental -> Maybe Ly.LyAccidental
olyAccidental Nat            = Nothing
olyAccidental Sharp          = Just Ly.sharp
olyAccidental Flat           = Just Ly.flat
olyAccidental DoubleSharp    = Just Ly.doubleSharp
olyAccidental DoubleFlat     = Just Ly.doubleFlat
     
olyOctaveSpec :: Int -> Maybe Ly.LyOctaveSpec
olyOctaveSpec i 
    | i > 0       = Just $ Ly.raised i
    | i < 0       = Just $ Ly.lowered (abs i)
    | otherwise   = Nothing 
    
olyDuration :: Duration -> Maybe Ly.LyDuration
olyDuration drn = let (n,d,dots) = durationElements drn 
                  in maybe Nothing (Just . addDots dots) (mkDuration n d)
  where 
    mkDuration 4 1      = Just Ly.longa  
    mkDuration 2 1      = Just Ly.breve 
    mkDuration 1 i      = Just $ Ly.duration i
    mkDuration _ _      = Nothing
    
    addDots :: Int -> Ly.LyDuration -> Ly.LyDuration
    addDots i d 
        | i > 0     = d ! (Ly.dotted i)
        | otherwise = d

    
--------------------------------------------------------------------------------
-- Midi

midiPitch :: Pitch -> Word8
midiPitch = fromIntegral . (+12) . semitones 

midiTicks :: Int -> Duration -> Int
midiTicks tpqn d = floor $ fromIntegral (4 * tpqn) * durationToDouble d 


    