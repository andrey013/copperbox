{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Pitch
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pitch representation.
--
--------------------------------------------------------------------------------

module Neume.Core.Pitch 
 ( 
 -- * Pitch types 
   Pitch(..)
 , PitchLetter(..)
 , Accidental(..)
 , Octave
 , PitchLabel(..)

 -- * Classes
 , HasPitch(..)

 -- * Operations
 , label
 , octave
 , makePitch
 , root
 , natural
 , toUpperLChar
 , toLowerLChar
 , setOctave
 , displaceOctave
 , lyOctaveDist
 , arithmeticDist

 -- * Pitch spelling
 , SpellingMap
 , spell
 , makeSpellingMap

 ) where

import Neume.Core.Utils.Pretty

import Text.PrettyPrint.Leijen          -- package: wl-pprint

import Data.Char ( toLower )
import qualified Data.Map as Map



-- | To print ABC,  Natural must be distinct from 
-- no-accidental. Hence the Maybe onAccidental.

data Pitch = Pitch PitchLetter (Maybe Accidental) Octave
  deriving (Eq,Show)

data PitchLetter = C | D | E | F | G | A | B
  deriving (Bounded,Enum,Eq,Ord,Show)

data Accidental = DoubleFlat | Flat | Nat | Sharp  | DoubleSharp 
  deriving (Bounded,Enum,Eq,Ord,Show)

type Octave  = Int

data PitchLabel = PitchLabel PitchLetter (Maybe Accidental)
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- Classes

class HasPitch p where
  getPitch :: p -> Pitch
  setPitch :: Pitch -> p -> p

instance HasPitch Pitch where
  getPitch = id
  setPitch = const


  
class HasSemitones a where 
  getSemitones :: a -> Int
    
instance HasSemitones Pitch where
  getSemitones (Pitch l a o) = getSemitones l + getSemitones a + (12 * o)

instance HasSemitones PitchLabel where
  getSemitones (PitchLabel l a) = getSemitones l + getSemitones a
  
instance HasSemitones a => HasSemitones (Maybe a) where
  getSemitones = maybe 0 getSemitones

instance HasSemitones PitchLetter where    
  getSemitones C = 0
  getSemitones D = 2
  getSemitones E = 4
  getSemitones F = 5
  getSemitones G = 7
  getSemitones A = 9
  getSemitones B = 11

instance HasSemitones Accidental where 
  getSemitones Nat          = 0
  getSemitones Sharp        = 1
  getSemitones Flat         = (-1)
  getSemitones DoubleSharp  = 2
  getSemitones DoubleFlat   = (-2)
  

-- Standard instances
  
instance Ord Pitch where
  compare p1 p2 = getSemitones p1 `compare` getSemitones p2

-- | The Ord instance of a Pitch label is not numerically sound, as 
-- it does not respect the semitone count. 
-- It is defined only to allow PitchLabels to be stored in a finite map.
instance Ord PitchLabel where
  compare (PitchLabel l a) (PitchLabel l' a') = (l,a) `compare` (l',a')


--------------------------------------------------------------------------------

-- | Extract the @PitchLabel@ from a @Pitch@.
label :: Pitch -> PitchLabel
label (Pitch l a _) = PitchLabel l a


-- | Extract the octave from a @Pitch@.
octave :: Pitch -> Octave
octave (Pitch _ _ o) = o

-- | Make a @Pitch@ with a @PitchLabel@ and an @Octave@ designation.
makePitch :: PitchLabel -> Octave -> Pitch
makePitch (PitchLabel l a) o = Pitch l a o

-- | Drop the accidental of a @PitchLabel@.
root :: PitchLabel -> PitchLabel
root (PitchLabel l _) = PitchLabel l Nothing

-- | Change the accidental of a @PitchLabel@ making it a
-- natural (when printed the natural sign will be appear 
-- as a cautaionary accidental). 
natural :: PitchLabel -> PitchLabel
natural (PitchLabel l _) = PitchLabel l (Just Nat)


-- | Print the PitchLetter as an upper case letter.
toUpperLChar :: PitchLetter -> Char  
toUpperLChar C         = 'C'  
toUpperLChar D         = 'D'
toUpperLChar E         = 'E'   
toUpperLChar F         = 'F'
toUpperLChar G         = 'G'   
toUpperLChar A         = 'A'
toUpperLChar B         = 'B'   

-- | Print the PitchLetter as a lower case letter.
toLowerLChar :: PitchLetter -> Char 
toLowerLChar = toLower . toUpperLChar


-- | Set the octave deginator:
-- @
--   LilyPond - middle c is c' (i.e. octave 1) 
--   Mullein  - middle c is c5 (i.e. octave 5)
-- @
setOctave :: Int -> Pitch -> Pitch
setOctave i (Pitch l a _)   = Pitch l a i

  
-- | Modify the octave designator, e.g displace by (-4) for  
-- LilyPond.
displaceOctave :: HasPitch e => Int -> e -> e
displaceOctave i e = let p = getPitch e in setPitch (fn p) e
  where 
    fn (Pitch l a o) = Pitch l a (o+i)



-- | Calculate the octave distance for LilyPond. 
-- The distance is modulo an interval of a fifth.
-- See Lilypond (6.1.6 - relative octaves)
-- @
--   ceses ->- fisis
--   cbb   ->- f##   -- fourth 
-- @
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



type SpellingMap = Map.Map PitchLabel PitchLabel


spell :: SpellingMap -> Pitch -> Pitch
spell sm p@(Pitch _ _ o) = makePitch (fn $ label p) o
  where
    fn lbl = maybe lbl id $ Map.lookup lbl sm

-- | Make a spelling map with @n@ accidentals. If @n@ is positive
-- the accidentals will be sharps, if @n@ s negative the 
-- accidentals will be flats.
makeSpellingMap :: Int -> SpellingMap
makeSpellingMap n 
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


--------------------------------------------------------------------------------
-- Pretty print instances

-- Pitch letters are upper case - then e.g. Bb is decipherable.

instance Pretty Pitch where
  pretty (Pitch p oa i) = pretty p <> (mbDoc pretty oa) <> int i

instance Pretty PitchLetter where
  pretty  = text . show


instance Pretty Accidental where
  pretty DoubleFlat   = text "bb"
  pretty Flat         = char 'b'
  pretty Nat          = empty
  pretty Sharp        = char '#'
  pretty DoubleSharp  = text "##"

instance Pretty PitchLabel where
  pretty (PitchLabel p oa) = pretty p <> (maybe empty pretty oa)









          