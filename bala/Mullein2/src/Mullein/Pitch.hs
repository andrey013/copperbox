{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Pitch
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pitch representation.
--
--------------------------------------------------------------------------------

module Mullein.Pitch where


import Data.Char ( toLower )
import qualified Data.Map as Map


type Octave  = Int

type SpellingMap = Map.Map PitchLabel PitchLabel


-- | To print ABC,  Natural must be distinct from 
-- no-accidental. Hence the Maybe onAccidental.

data Pitch = Pitch PitchLetter (Maybe Accidental) Octave
  deriving (Eq,Show)

data PitchLetter = C | D | E | F | G | A | B
  deriving (Bounded,Enum,Eq,Ord,Show)

data Accidental = DoubleFlat | Flat | Nat | Sharp  | DoubleSharp 
  deriving (Bounded,Enum,Eq,Ord,Show)

data PitchLabel = PitchLabel PitchLetter (Maybe Accidental)
  deriving (Eq,Show)
  
  
instance Ord Pitch where
  compare p1 p2 = semitones p1 `compare` semitones p2


instance Enum PitchLabel where 
  fromEnum (PitchLabel l a) = (fn l + semitones a) `mod` 12
    where fn C = 0
          fn D = 2
          fn E = 4
          fn F = 5
          fn G = 7
          fn A = 9
          fn B = 11

  
  toEnum 0    = PitchLabel C Nothing
  toEnum 1    = PitchLabel C (Just Sharp)
  toEnum 2    = PitchLabel D Nothing
  toEnum 3    = PitchLabel D (Just Sharp)
  toEnum 4    = PitchLabel E Nothing
  toEnum 5    = PitchLabel F Nothing
  toEnum 6    = PitchLabel F (Just Sharp)
  toEnum 7    = PitchLabel G Nothing
  toEnum 8    = PitchLabel G (Just Sharp)
  toEnum 9    = PitchLabel A Nothing
  toEnum 10   = PitchLabel A (Just Sharp)
  toEnum 11   = PitchLabel B Nothing
  toEnum i    = error $ "Pitch.toEnum " ++ show i ++ " outside bounds"
  
instance Bounded PitchLabel where
  maxBound = toEnum 11
  minBound = toEnum 0


-- | The Ord instance of a Pitch label is not numerically sound, as 
-- it does not respect the semitone count. 
-- It is defined only to allow PitchLabels to be stored in a finite map.
instance Ord PitchLabel where
  compare (PitchLabel l a) (PitchLabel l' a') = (l,a) `compare` (l',a')


-- No equivalent to HasDuration for Pitch as cardinality is
-- a problem.
-- For instance, chords have more than one pitch, rests have no pitch


label :: Pitch -> PitchLabel
label (Pitch l a _) = PitchLabel l a

octave :: Pitch -> Octave
octave (Pitch _ _ o) =o

pitch :: PitchLabel -> Octave -> Pitch
pitch (PitchLabel l a) o = Pitch l a o

root :: PitchLabel -> PitchLabel
root (PitchLabel l _) = PitchLabel l Nothing

natural :: PitchLabel -> PitchLabel
natural (PitchLabel l _) = PitchLabel l (Just Nat)


-- | Provide fmap-like access to the Pitch component embedded 
-- in some arbitrary structure.
class PitchMap e where
  pitchMap  :: (Pitch -> Pitch) -> e -> e 
  pitchMapM :: Monad m => (Pitch -> m Pitch) -> e -> m e

instance PitchMap Pitch where
  pitchMap = ($)
  pitchMapM mf p = mf p   

  
class Semitones a where semitones :: a -> Int
    
instance Semitones Pitch where
  semitones (Pitch l a o) = semitones l + semitones a + (12 * o)

instance Semitones PitchLabel where
  semitones (PitchLabel l a) = semitones l + semitones a
  
instance Semitones a => Semitones (Maybe a) where
  semitones = maybe 0 semitones

  
toUpperLChar :: PitchLetter -> Char  
toUpperLChar C         = 'C'  
toUpperLChar D         = 'D'
toUpperLChar E         = 'E'   
toUpperLChar F         = 'F'
toUpperLChar G         = 'G'   
toUpperLChar A         = 'A'
toUpperLChar B         = 'B'   

toLowerLChar :: PitchLetter -> Char 
toLowerLChar = toLower . toUpperLChar



-- LilyPond - middle c is c' (i.e. octave 1) 
-- Mullein  - middle c is c5 (i.e. octave 5)
rescaleOctave :: Int -> Pitch -> Pitch
rescaleOctave i (Pitch l a o)   = Pitch l a (o+i)

  


instance Semitones PitchLetter where    
  semitones C = 0
  semitones D = 2
  semitones E = 4
  semitones F = 5
  semitones G = 7
  semitones A = 9
  semitones B = 11

instance Semitones Accidental where 
  semitones Nat          = 0
  semitones Sharp        = 1
  semitones Flat         = (-1)
  semitones DoubleSharp  = 2
  semitones DoubleFlat   = (-2)



-- See Lilypond (6.1.6 - relative octaves)
-- ceses ->- fisis
-- cbb   ->- f##   -- fourth 
lyOctaveDist :: Pitch -> Pitch -> Int
lyOctaveDist p p' = sign . fn . (`divMod` 7) . abs $ arithmeticDist p p'
  where
    fn (d,m) | m > 4              = 1 + d
             | otherwise          = d
    sign a | p <= p'              = a
           | otherwise            = negate a          

-- | The arithmetic distance between pitches is a /retrograde/ count of 
-- the pitch letters. Retrograde meaning that the starting letter is 
-- counted e.g. the distance from C4 to C4 is 1 not 0. 
arithmeticDist :: Pitch -> Pitch -> Int
arithmeticDist p p' = retro $ lexval p' - lexval p
  where
    retro i | i >= 0   = i+1
            |otherwise = i-1

    -- 'lexical' value, i.e Pitch value without accidental alteration.  
    lexval (Pitch l _ o) = fromEnum l + 7*o


--------------------------------------------------------------------------------
-- Pitch spelling for ABC

{-
-- 7 sharps:   C#      A#m      G#Mix   D#Dor   E#Phr   F#Lyd   B#Loc
-- 6 sharps:   F#      D#m      C#Mix   G#Dor   A#Phr   BLyd    E#Loc
-- 5 sharps:   B       G#m      F#Mix   C#Dor   D#Phr   ELyd    A#Loc
-- 4 sharps:   E       C#m      BMix    F#Dor   G#Phr   ALyd    D#Loc
-- 3 sharps:   A       F#m      EMix    BDor    C#Phr   DLyd    G#Loc
-- 2 sharps:   D       Bm       AMix    EDor    F#Phr   GLyd    C#Loc
-- 1 sharp :   G       Em       DMix    ADor    BPhr    CLyd    F#Loc
-- 0 sharps:   C       Am       GMix    DDor    EPhr    FLyd    BLoc
-- 1 flat  :   F       Dm       CMix    GDor    APhr    BbLyd   ELoc
-- 2 flats :   Bb      Gm       FMix    CDor    DPhr    EbLyd   ALoc
-- 3 flats :   Eb      Cm       BbMix   FDor    GPhr    AbLyd   DLoc
-- 4 flats :   Ab      Fm       EbMix   BbDor   CPhr    DbLyd   GLoc
-- 5 flats :   Db      Bbm      AbMix   EbDor   FPhr    GbLyd   CLoc
-- 6 flats :   Gb      Ebm      DbMix   AbDor   BbPhr   CbLyd   FLoc
-- 7 flats :   Cb      Abm      GbMix   DbDor   EbPhr   FbLyd   BbLoc
-}




spell :: PitchMap e => SpellingMap -> e -> e
spell sm = pitchMap (spell' sm)

spell' :: SpellingMap -> Pitch -> Pitch
spell' sm p@(Pitch _ _ o) = pitch (fn $ label p) o
  where
    fn lbl = maybe lbl id $ Map.lookup lbl sm


spellingMap :: Int -> SpellingMap
spellingMap n 
    | abs n > 7 = error "Pitch.spellingMap - more sharps/flats than notes."
    | n == 0    = Map.empty
    | n >  0    = build $ nsharps n
    | otherwise = build $ nflats (abs n)          
  where
    build = foldr fn Map.empty where
      fn lbl m = Map.insert (root lbl) (natural lbl) 
               $ Map.insert lbl (root lbl) m

nsharps :: Int -> [PitchLabel]
nsharps n = map sharp $ take n [F,C,G,D,A,E,B] where
  sharp l = PitchLabel l (Just Sharp)

nflats :: Int -> [PitchLabel]
nflats n = map flat $ take n [B,E,A,D,G,C,F] where
  flat l = PitchLabel l (Just Flat)


    
    








          