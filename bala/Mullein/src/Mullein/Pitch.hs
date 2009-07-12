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

module Mullein.Pitch 
  ( 
  -- * Pitch types
    Pitch(..)
  , Octave
  , PitchLetter(..)
  , Accidental(..)
  , PitchLabel(..)

  -- * Type classes
  , Semitones(..)

  , transpose


  , toUpperLChar
  , toLowerLChar


  , rescaleOctave
  , octaveDist

  ) where


import Data.Char ( toLower )

import qualified Text.PrettyPrint.Leijen as PP

type Octave  = Int

data Pitch = Pitch PitchLetter Accidental Octave
  deriving (Eq,Show)

data PitchLetter = C | D | E | F | G | A | B
  deriving (Bounded,Enum,Eq,Ord,Show)

data Accidental = DoubleFlat | Flat | Nat | Sharp  | DoubleSharp 
  deriving (Bounded,Enum,Eq,Ord,Show)

data PitchLabel = PitchLabel PitchLetter Accidental
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


  
class Semitones a where semitones :: a -> Int
    
instance Semitones Pitch where
  semitones (Pitch l a o) = semitones l + semitones a + (12 * o)

instance Semitones PitchLabel where
  semitones (PitchLabel l a) = semitones l + semitones a
  
  

  
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

  
  
-- This will need pitch spelling
fromSemitones :: Int -> Pitch
fromSemitones i = Pitch l a o
  where
    (o,ni)      = i `divMod` 12
    (l,a)       = pitchVal ni                   
                 
    pitchVal  0 = (C,Nat)
    pitchVal  1 = (C,Sharp)
    pitchVal  2 = (D,Nat)
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

-- | Traspose a pitch - note the result may have an unorthodox 
-- pitch spelling. You should respell the result with respect to
-- a scale after this operation. 
transpose :: Int -> Pitch -> Pitch
transpose i p = fromSemitones $ i + semitones p

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
octaveDist p p' = sign . fn . (`divMod` 7) . abs $ arithmeticDist p p'
  where
    fn (d,m) | d >=0 && m > 4     = 1 + d  -- only for postive numbers
             | otherwise          = d
    sign a | p <= p'              = a
           | otherwise            = negate a          
             
arithmeticDist :: Pitch -> Pitch -> Int
arithmeticDist (Pitch l _ o) (Pitch l' _ o') = 
    dist (o * 7 + fromEnum l) (o' * 7 + fromEnum l')
  where
    dist i i'
      | i > i'      = negate $ 1 + (i - i')
      | otherwise   = 1 + (i' - i)


  
--------------------------------------------------------------------------------
-- pretty print 

instance PP.Pretty Pitch where
  pretty (Pitch l a o)  = PP.pretty l PP.<> PP.pretty a PP.<> PP.int o

instance PP.Pretty PitchLetter where
  pretty         = PP.char . toLowerLChar 


instance PP.Pretty Accidental where
  pretty Nat          = PP.empty
  pretty Sharp        = PP.char 's'
  pretty Flat         = PP.char 'f'
  pretty DoubleSharp  = PP.text "ss"
  pretty DoubleFlat   = PP.text "ff"


    
    








          