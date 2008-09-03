
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Pitch
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A minimal representation for Pitch and Duration.
--
--------------------------------------------------------------------------------

module HNotate.Pitch (
    -- Internal types
    Pitch(..),
    PitchLetter(..),
    Accidental(..),

    -- * Operations

    semitones,
    arithmeticDistance,
    
    -- * lilyPond helpers
    middleC, octaveDist,
    
    
    -- * pretty print
    ppNote,

    -- * Named elements
    -- $nameddoc 

    middle_c,
    c4, d4, e4, f4, g4, a4, b4,
    c4is, d4es, d4is, e4es, f4is, g4es, g4is, a4es, a4is, b4es,
    
    c3, d3, e3, f3, g3, a3, b3,
    c3is, d3es, d3is, e3es, f3is, g3es, g3is, a3es, a3is, b3es,
  
    c2, d2, e2, f2, g2, a2, b2,
    c2is, d2es, d2is, e2es, f2is, g2es, g2is, a2es, a2is, b2es,
  
    c1, d1, e1, f1, g1, a1, b1,
    c1is, d1es, d1is, e1es, f1is, g1es, g1is, a1es, a1is, b1es,
        
    c5, d5, e5, f5, g5, a5, b5,
    c5is, d5es, d5is, e5es, f5is, g5es, g5is, a5es, a5is, b5es,
    
    c6, d6, e6, f6, g6, a6, b6,
    c6is, d6es, d6is, e6es, f6is, g6es, g6is, a6es, a6is, b6es,
  
    c7, d7, e7, f7, g7, a7, b7,
    c7is, d7es, d7is, e7es, f7is, g7es, g7is, a7es, a7is, b7es

    
  ) where

import Data.Word
import Text.PrettyPrint.Leijen

data Pitch = Pitch {
    pitch_letter  :: PitchLetter,
    accidental    :: Accidental,
    octave        :: Int
  }
  deriving (Eq)

data PitchLetter = C | D | E | F | G | A | B
  deriving (Eq,Enum,Ord,Show)

data Accidental = DoubleFlat | Flat | Nat | Sharp  | DoubleSharp 
  deriving (Eq,Enum,Ord,Show)

instance Show Pitch where
  show = show . pretty
  
instance Ord Pitch where
  compare p1 p2 = semitones p1 `compare` semitones p2


semitones :: Pitch -> Int
semitones (Pitch l a o) = semis l + asemis a + (12 * o)

semis C = 0
semis D = 2
semis E = 4
semis F = 5
semis G = 7
semis A = 9
semis B = 11

asemis Nat          = 0
asemis Sharp        = 1
asemis Flat         = (-1)
asemis DoubleSharp  = 2
asemis DoubleFlat   = (-2)

-- LilPond Helpers
middleC :: Pitch
middleC = Pitch C Nat 4



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


-- Helpers for Midi

    
    



--------------------------------------------------------------------------------
-- Pretty instances


instance Pretty Pitch where
  pretty (Pitch l a o)  = group $ pretty l <> pretty a <> int o

instance Pretty PitchLetter where
  pretty              = text . show

instance Pretty Accidental where
  pretty Nat          = empty
  pretty Sharp        = char '#'
  pretty Flat         = char 'b'
  pretty DoubleSharp  = text "##"
  pretty DoubleFlat   = text "bb"


ppNote pch dur = group $
      pretty pch <> char '/' <> pretty dur

--------------------------------------------------------------------------------
-- Named elements
-- $nameddoc 
-- Pre-defined pitches. Middle c is @c4@, octaves start on c. 
-- Sharp and flat notes follow the LilyPond convention with suffix of @is@ for 
-- a sharp and @es@ for a flat.
      
pchNat n o    = Pitch n Nat o
pchSharp n o  = Pitch n Sharp o
pchFlat n o   = Pitch n Flat o

middle_c :: Pitch
middle_c = pchNat C 4

c4, d4, e4, f4, g4, a4, b4, 
    c4is, d4es, d4is, e4es, f4is, g4es, g4is, a4es, a4is, b4es :: Pitch
c4    = pchNat C 4
d4    = pchNat D 4
e4    = pchNat E 4
f4    = pchNat F 4
g4    = pchNat G 4
a4    = pchNat A 4
b4    = pchNat B 4
c4is  = pchSharp C 4
d4es  = pchFlat D 4
d4is  = pchSharp D 4
e4es  = pchFlat E 4
f4is  = pchSharp F 4
g4es  = pchFlat G 4
g4is  = pchSharp G 4
a4es  = pchFlat A 4
a4is  = pchSharp A 4
b4es  = pchFlat B 4


c3, d3, e3, f3, g3, a3, b3, 
    c3is, d3es, d3is, e3es, f3is, g3es, g3is, a3es, a3is, b3es :: Pitch
c3    = pchNat C 3
d3    = pchNat D 3
e3    = pchNat E 3
f3    = pchNat F 3
g3    = pchNat G 3
a3    = pchNat A 3
b3    = pchNat B 3
c3is  = pchSharp C 3
d3es  = pchFlat D 3
d3is  = pchSharp D 3
e3es  = pchFlat E 3
f3is  = pchSharp F 3
g3es  = pchFlat G 3
g3is  = pchSharp G 3
a3es  = pchFlat A 3
a3is  = pchSharp A 3
b3es  = pchFlat B 3

c2, d2, e2, f2, g2, a2, b2, 
    c2is, d2es, d2is, e2es, f2is, g2es, g2is, a2es, a2is, b2es :: Pitch
c2    = pchNat C 2
d2    = pchNat D 2
e2    = pchNat E 2
f2    = pchNat F 2
g2    = pchNat G 2
a2    = pchNat A 2
b2    = pchNat B 2
c2is  = pchSharp C 2
d2es  = pchFlat D 2
d2is  = pchSharp D 2
e2es  = pchFlat E 2
f2is  = pchSharp F 2
g2es  = pchFlat G 2
g2is  = pchSharp G 2
a2es  = pchFlat A 2
a2is  = pchSharp A 2
b2es  = pchFlat B 2

c1, d1, e1, f1, g1, a1, b1, 
    c1is, d1es, d1is, e1es, f1is, g1es, g1is, a1es, a1is, b1es :: Pitch
c1    = pchNat C 1
d1    = pchNat D 1
e1    = pchNat E 1
f1    = pchNat F 1
g1    = pchNat G 1
a1    = pchNat A 1
b1    = pchNat B 1
c1is  = pchSharp C 1
d1es  = pchFlat D 1
d1is  = pchSharp D 1
e1es  = pchFlat E 1
f1is  = pchSharp F 1
g1es  = pchFlat G 1
g1is  = pchSharp G 1
a1es  = pchFlat A 1
a1is  = pchSharp A 1
b1es  = pchFlat B 1


c5, d5, e5, f5, g5, a5, b5, 
    c5is, d5es, d5is, e5es, f5is, g5es, g5is, a5es, a5is, b5es :: Pitch
c5    = pchNat C 5
d5    = pchNat D 5
e5    = pchNat E 5
f5    = pchNat F 5
g5    = pchNat G 5
a5    = pchNat A 5
b5    = pchNat B 5
c5is  = pchSharp C 5
d5es  = pchFlat D 5
d5is  = pchSharp D 5
e5es  = pchFlat E 5
f5is  = pchSharp F 5
g5es  = pchFlat G 5
g5is  = pchSharp G 5
a5es  = pchFlat A 5
a5is  = pchSharp A 5
b5es  = pchFlat B 5

c6, d6, e6, f6, g6, a6, b6, 
    c6is, d6es, d6is, e6es, f6is, g6es, g6is, a6es, a6is, b6es :: Pitch
c6    = pchNat C 6
d6    = pchNat D 6
e6    = pchNat E 6
f6    = pchNat F 6
g6    = pchNat G 6
a6    = pchNat A 6
b6    = pchNat B 6
c6is  = pchSharp C 6
d6es  = pchFlat D 6
d6is  = pchSharp D 6
e6es  = pchFlat E 6
f6is  = pchSharp F 6
g6es  = pchFlat G 6
g6is  = pchSharp G 6
a6es  = pchFlat A 6
a6is  = pchSharp A 6
b6es  = pchFlat B 6

c7, d7, e7, f7, g7, a7, b7, 
    c7is, d7es, d7is, e7es, f7is, g7es, g7is, a7es, a7is, b7es :: Pitch
c7    = pchNat C 7
d7    = pchNat D 7
e7    = pchNat E 7
f7    = pchNat F 7
g7    = pchNat G 7
a7    = pchNat A 7
b7    = pchNat B 7
c7is  = pchSharp C 7
d7es  = pchFlat D 7
d7is  = pchSharp D 7
e7es  = pchFlat E 7
f7is  = pchSharp F 7
g7es  = pchFlat G 7
g7is  = pchSharp G 7
a7es  = pchFlat A 7
a7is  = pchSharp A 7
b7es  = pchFlat B 7


          