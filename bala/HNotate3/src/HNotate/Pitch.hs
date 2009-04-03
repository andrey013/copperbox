{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Pitch
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pitch representation.
--
--------------------------------------------------------------------------------

module HNotate.Pitch (
    -- Internal types
    Pitch(..),
    PitchLetter(..),
    Accidental(..),
    PitchLabel(..),

    PitchContent,
    noPitchContent, 
    
    -- * Operations
    PitchValue(..),
    Semitones(..),
    
    fromLChar, toUpperLChar, toLowerLChar,

    
    fromSemitones,
    arithmeticDistance,

    octaveConst, accidentalConst,
        
    -- * LilyPond helpers
    octaveDist, no_octave,





    
  ) where


-- Avoid internal dependencies as this module!

import Data.Char (toUpper, toLower)
import qualified Data.Foldable as F
import Data.Generics
import Data.Sequence hiding (length)
import qualified Data.Sequence as S

import qualified Text.PrettyPrint.Leijen as PP

data Pitch = Pitch {
    pch_letter        :: PitchLetter,
    pch_accidental    :: Accidental,
    pch_octave        :: Int
  }
  deriving (Eq,Data,Show,Typeable)

data PitchLetter = C | D | E | F | G | A | B
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)

data Accidental = DoubleFlat | Flat | Nat | Sharp  | DoubleSharp 
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)

data PitchLabel = PitchLabel {
    pch_lbl_letter      :: PitchLetter,
    pch_lbl_accidental  :: Accidental
  }
  deriving (Eq,Data,Show,Typeable)
  
  
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

  
  toEnum 0    = PitchLabel C Nat
  toEnum 1    = PitchLabel C Sharp
  toEnum 2    = PitchLabel D Nat
  toEnum 3    = PitchLabel D Sharp
  toEnum 4    = PitchLabel E Nat
  toEnum 5    = PitchLabel F Nat
  toEnum 6    = PitchLabel F Sharp
  toEnum 7    = PitchLabel G Nat
  toEnum 8    = PitchLabel G Sharp
  toEnum 9    = PitchLabel A Nat
  toEnum 10   = PitchLabel A Sharp
  toEnum 11   = PitchLabel B Nat
  toEnum i    = error $ "Pitch.toEnum " ++ show i ++ " outside bounds"
  
instance Bounded PitchLabel where
  maxBound = toEnum 11
  minBound = toEnum 0


-- Unlike rhythmicValue, pitchValue is partial
-- \no-rhythmic-value\ is effective synonymous with \no-duration\ (aka 0)
-- But there is no effective \no-pitch\ - pitch 0 is actually C natural four
-- octaves below middle C.
-- Also chords (and graces notes) a generally treated as an indivisible 
-- entity, but the have multiple pitches
-- Hence we treat PitchValue as a list.  
type PitchContent = [Pitch]

noPitchContent :: PitchContent
noPitchContent = []

class PitchValue a where
  pitchValue   :: a -> PitchContent
  updatePitch  :: PitchContent -> a -> a 

instance PitchValue Pitch where
  pitchValue p = [p]
  
  updatePitch [p] _ = p
  updatePitch _   p = p     -- ideally this case would never match...

instance PitchValue (Seq Pitch) where
  pitchValue = F.toList
  
  updatePitch pc se 
      | S.length se == length pc  = step (viewl se) pc 
      | otherwise                 = error "modifyPitch (Seq Pitch) unmatched"
    where
      step (_ :< sa) (b:bs)   = b <| step (viewl sa) bs
      step (a :< sa) []       = a <| sa
      step _         _        = empty
  
  
          
  
class Semitones a where semitones :: a -> Int
    
instance Semitones Pitch where
  semitones (Pitch l a o) = semitones l + semitones a + (12 * o)

instance Semitones PitchLabel where
  semitones (PitchLabel l a) = semitones l + semitones a
  
  

fromLChar :: Char -> Maybe PitchLetter 
fromLChar = letter . toUpper 
  where      
    letter 'C'    = Just C   
    letter 'D'    = Just D
    letter 'E'    = Just E   
    letter 'F'    = Just F
    letter 'G'    = Just G   
    letter 'A'    = Just A
    letter 'B'    = Just B   
    letter _      = Nothing
  
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


  
  
-- This will need pitch spelling
fromSemitones :: Int -> Pitch
fromSemitones i = let (o,ni) = i `divMod` 12
                      (l,a)  = pitchVal ni                   
                  in Pitch l a o
  where
    pitchVal  0 = (C,Nat)
    pitchVal  1 = (C,Sharp)
    pitchVal  2 = (D,Sharp)
    pitchVal  3 = (D,Sharp)
    pitchVal  4 = (E,Nat)
    pitchVal  5 = (F,Nat)
    pitchVal  6 = (F,Sharp)
    pitchVal  7 = (G,Nat)
    pitchVal  8 = (G,Sharp)
    pitchVal  9 = (A,Nat)
    pitchVal 10 = (A,Sharp)
    pitchVal 11 = (B,Nat)
    pitchVal _  = error "fromSemitones - not unreachable after all!" 

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
octaveDist :: Pitch -> Pitch -> Int
octaveDist p p' =
    fn (abs $ arithmeticDistance p p') (if p > p' then negate else id) 
  where
    fn dist f 
      | dist <= 4 = 0
      | otherwise = f $ (dist - 4) `div` 6 + 1 
   
arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance (Pitch l _ o) (Pitch l' _ o') = 
    dist (o * 7 + fromEnum l) (o' * 7 + fromEnum l')
  where
    dist i i'
      | i > i'      = negate $ 1 + (i - i')
      | otherwise   = 1 + (i' - i)

no_octave :: Int 
no_octave = minBound

-- Helpers for Abc

-- | Set the octave value (as per const this forgets the original value).
octaveConst :: Pitch -> Int -> Pitch
octaveConst (Pitch l a _) o = Pitch l a o

-- | Set the accidental value (as per const this forgets the original value).
accidentalConst :: Pitch -> Accidental -> Pitch
accidentalConst (Pitch l _ o) a = Pitch l a o


  
--------------------------------------------------------------------------------
-- pretty print 

instance PP.Pretty Pitch where
  pretty (Pitch l a o)  = PP.pretty l PP.<> PP.pretty a PP.<> PP.int o

instance PP.Pretty PitchLetter where
  pretty         = PP.text . show

instance PP.Pretty Accidental where
  pretty Nat          = PP.empty
  pretty Sharp        = PP.char '#'
  pretty Flat         = PP.char 'b'
  pretty DoubleSharp  = PP.text "##"
  pretty DoubleFlat   = PP.text "bb"


    
    








          