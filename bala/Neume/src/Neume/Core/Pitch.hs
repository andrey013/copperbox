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
-- Middle C is C4!
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


  -- * Constants
  , middle_c

  -- * Operations
  , label
  , toUpperLChar
  , toLowerLChar
  , setOctave
  , displaceOctave
  , lyOctaveDist
  , arithmeticDist

  ) where

import Neume.Core.Utils.Pretty

import Text.PrettyPrint.Leijen          -- package: wl-pprint

import Data.Char ( toLower )



-- | To print ABC,  Natural must be distinct from 
-- no-accidental. Hence the Maybe onAccidental.

data Pitch = Pitch 
      { pitch_letter    :: PitchLetter 
      , opt_accidental  :: Maybe Accidental
      , octave          :: Octave
      }
  deriving (Eq)

data PitchLetter = C | D | E | F | G | A | B
  deriving (Bounded,Enum,Eq,Ord,Show)

data Accidental = DoubleFlat | Flat | Nat | Sharp  | DoubleSharp 
  deriving (Bounded,Enum,Eq,Ord)

type Octave  = Int

data PitchLabel = PitchLabel PitchLetter (Maybe Accidental)
  deriving (Eq)

--------------------------------------------------------------------------------
-- Ord instance


    
semitoneCount :: Pitch -> Int
semitoneCount (Pitch l mba o) = 
    letterSemis l + maybe 0 accidentalSemis mba + (12 * o)
  where
    letterSemis C               = 0
    letterSemis D               = 2
    letterSemis E               = 4
    letterSemis F               = 5
    letterSemis G               = 7
    letterSemis A               = 9
    letterSemis B               = 11

    accidentalSemis Nat         = 0
    accidentalSemis Sharp       = 1
    accidentalSemis Flat        = (-1)
    accidentalSemis DoubleSharp = 2
    accidentalSemis DoubleFlat  = (-2)
  

-- Standard instances
  
instance Ord Pitch where
  compare p1 p2 = semitoneCount p1 `compare` semitoneCount p2



-- | The Ord instance of a Pitch label is not numerically sound, as 
-- it does not respect the semitone count. 
-- It is defined only to allow PitchLabels to be stored in a finite map.
instance Ord PitchLabel where
  compare (PitchLabel l a) (PitchLabel l' a') = (l,a) `compare` (l',a')



--------------------------------------------------------------------------------


-- | Middle c is C4 - as per ANSI pitch notation.
--
middle_c :: Pitch
middle_c = Pitch C Nothing 4



-- | Extract the @PitchLabel@ from a @Pitch@.
label :: Pitch -> PitchLabel
label (Pitch l a _) = PitchLabel l a


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
displaceOctave :: Int -> Pitch -> Pitch
displaceOctave i (Pitch l a o) = Pitch l a (o+i)


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
-- Show instances
instance Show Pitch where
  showsPrec _ (Pitch l mba i) = shows l . maybe id shows mba  . shows i


instance Show PitchLabel where
  showsPrec _ (PitchLabel l mba) = shows l . maybe id shows mba

instance Show Accidental where
  showsPrec _ DoubleFlat   = showString "bb"
  showsPrec _ Flat         = showChar 'b'
  showsPrec _ Nat          = id
  showsPrec _ Sharp        = showChar '#'
  showsPrec _ DoubleSharp  = showString "##"


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





