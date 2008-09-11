
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
-- Pitch representation.
--
--------------------------------------------------------------------------------

module HNotate.Pitch (
    -- Internal types
    Pitch(..),
    PitchLetter(..),
    Accidental(..),

    -- * Operations

    semitones,
    fromSemitones,
    arithmeticDistance,
    
    -- * lilyPond helpers
    middleC, octaveDist,
    
    
    -- * pretty print
    ppNote,

    -- * Named elements
    -- $nameddoc 

    middle_c,
    c4, d4, e4, f4, g4, a4, b4,
    cis4, des4, dis4, ees4, fis4, ges4, gis4, aes4, ais4, bes4,
    
    c3, d3, e3, f3, g3, a3, b3,
    cis3, des3, dis3, ees3, fis3, ges3, gis3, aes3, ais3, bes3,
  
    c2, d2, e2, f2, g2, a2, b2,
    cis2, des2, dis2, ees2, fis2, ges2, gis2, aes2, ais2, bes2,
  
    c1, d1, e1, f1, g1, a1, b1,
    cis1, des1, dis1, ees1, fis1, ges1, gis1, aes1, ais1, bes1,
        
    c5, d5, e5, f5, g5, a5, b5,
    cis5, des5, dis5, ees5, fis5, ges5, gis5, aes5, ais5, bes5,
    
    c6, d6, e6, f6, g6, a6, b6,
    cis6, des6, dis6, ees6, fis6, ges6, gis6, aes6, ais6, bes6,
  
    c7, d7, e7, f7, g7, a7, b7,
    cis7, des7, dis7, ees7, fis7, ges7, gis7, aes7, ais7, bes7

    
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
    cis4, des4, dis4, ees4, fis4, ges4, gis4, aes4, ais4, bes4 :: Pitch
c4    = pchNat C 4
d4    = pchNat D 4
e4    = pchNat E 4
f4    = pchNat F 4
g4    = pchNat G 4
a4    = pchNat A 4
b4    = pchNat B 4
cis4  = pchSharp C 4
des4  = pchFlat D 4
dis4  = pchSharp D 4
ees4  = pchFlat E 4
fis4  = pchSharp F 4
ges4  = pchFlat G 4
gis4  = pchSharp G 4
aes4  = pchFlat A 4
ais4  = pchSharp A 4
bes4  = pchFlat B 4


c3, d3, e3, f3, g3, a3, b3, 
    cis3, des3, dis3, ees3, fis3, ges3, gis3, aes3, ais3, bes3 :: Pitch
c3    = pchNat C 3
d3    = pchNat D 3
e3    = pchNat E 3
f3    = pchNat F 3
g3    = pchNat G 3
a3    = pchNat A 3
b3    = pchNat B 3
cis3  = pchSharp C 3
des3  = pchFlat D 3
dis3  = pchSharp D 3
ees3  = pchFlat E 3
fis3  = pchSharp F 3
ges3  = pchFlat G 3
gis3  = pchSharp G 3
aes3  = pchFlat A 3
ais3  = pchSharp A 3
bes3  = pchFlat B 3

c2, d2, e2, f2, g2, a2, b2, 
    cis2, des2, dis2, ees2, fis2, ges2, gis2, aes2, ais2, bes2 :: Pitch
c2    = pchNat C 2
d2    = pchNat D 2
e2    = pchNat E 2
f2    = pchNat F 2
g2    = pchNat G 2
a2    = pchNat A 2
b2    = pchNat B 2
cis2  = pchSharp C 2
des2  = pchFlat D 2
dis2  = pchSharp D 2
ees2  = pchFlat E 2
fis2  = pchSharp F 2
ges2  = pchFlat G 2
gis2  = pchSharp G 2
aes2  = pchFlat A 2
ais2  = pchSharp A 2
bes2  = pchFlat B 2

c1, d1, e1, f1, g1, a1, b1, 
    cis1, des1, dis1, ees1, fis1, ges1, gis1, aes1, ais1, bes1 :: Pitch
c1    = pchNat C 1
d1    = pchNat D 1
e1    = pchNat E 1
f1    = pchNat F 1
g1    = pchNat G 1
a1    = pchNat A 1
b1    = pchNat B 1
cis1  = pchSharp C 1
des1  = pchFlat D 1
dis1  = pchSharp D 1
ees1  = pchFlat E 1
fis1  = pchSharp F 1
ges1  = pchFlat G 1
gis1  = pchSharp G 1
aes1  = pchFlat A 1
ais1  = pchSharp A 1
bes1  = pchFlat B 1


c5, d5, e5, f5, g5, a5, b5, 
    cis5, des5, dis5, ees5, fis5, ges5, gis5, aes5, ais5, bes5 :: Pitch
c5    = pchNat C 5
d5    = pchNat D 5
e5    = pchNat E 5
f5    = pchNat F 5
g5    = pchNat G 5
a5    = pchNat A 5
b5    = pchNat B 5
cis5  = pchSharp C 5
des5  = pchFlat D 5
dis5  = pchSharp D 5
ees5  = pchFlat E 5
fis5  = pchSharp F 5
ges5  = pchFlat G 5
gis5  = pchSharp G 5
aes5  = pchFlat A 5
ais5  = pchSharp A 5
bes5  = pchFlat B 5

c6, d6, e6, f6, g6, a6, b6, 
    cis6, des6, dis6, ees6, fis6, ges6, gis6, aes6, ais6, bes6 :: Pitch
c6    = pchNat C 6
d6    = pchNat D 6
e6    = pchNat E 6
f6    = pchNat F 6
g6    = pchNat G 6
a6    = pchNat A 6
b6    = pchNat B 6
cis6  = pchSharp C 6
des6  = pchFlat D 6
dis6  = pchSharp D 6
ees6  = pchFlat E 6
fis6  = pchSharp F 6
ges6  = pchFlat G 6
gis6  = pchSharp G 6
aes6  = pchFlat A 6
ais6  = pchSharp A 6
bes6  = pchFlat B 6

c7, d7, e7, f7, g7, a7, b7, 
    cis7, des7, dis7, ees7, fis7, ges7, gis7, aes7, ais7, bes7 :: Pitch
c7    = pchNat C 7
d7    = pchNat D 7
e7    = pchNat E 7
f7    = pchNat F 7
g7    = pchNat G 7
a7    = pchNat A 7
b7    = pchNat B 7
cis7  = pchSharp C 7
des7  = pchFlat D 7
dis7  = pchSharp D 7
ees7  = pchFlat E 7
fis7  = pchSharp F 7
ges7  = pchFlat G 7
gis7  = pchSharp G 7
aes7  = pchFlat A 7
ais7  = pchSharp A 7
bes7  = pchFlat B 7


          