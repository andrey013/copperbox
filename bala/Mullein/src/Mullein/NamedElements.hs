{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.NamedElements
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Named musical eleemnts e.g. notes, keys...
--
--------------------------------------------------------------------------------

module Mullein.NamedElements (

    -- * Named pitches
    -- $pitchdoc 
    c_nat, d_nat, e_nat, f_nat, g_nat, a_nat, b_nat, 
    c_sharp, d_sharp, f_sharp, g_sharp, a_sharp, 
    d_flat, e_flat, g_flat, a_flat, b_flat,


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
    cis7, des7, dis7, ees7, fis7, ges7, gis7, aes7, ais7, bes7,
    
    -- * Named durations (American)
    longa, 
    -- $amerdoc
    double_whole, whole, half, quarter, eighth, sixteenth, thirty_second,
    sixty_fourth, one_hundred_twenty_eighth,
    
    -- * Named durations (English)
    -- $engdoc
    breve, semibreve, minim, crochet, quaver, semiquaver, demisemiquaver,
    hemidemisemiquaver, semihemidemisemiquaver,
    
    -- * Named durations (Shorthand)
    du1, du2, du4, du8, du16, du32, du64, du128,  
    
    -- * Named keys
    -- ** Major
    c_major, g_major, d_major, a_major, e_major, b_major, f_sharp_major,
    c_sharp_major,
    f_major, b_flat_major, e_flat_major, a_flat_major, d_flat_major,
    g_flat_major, c_flat_major,
    
    -- ** Minor
    a_minor, e_minor, b_minor, 
    f_sharp_minor, c_sharp_minor, g_sharp_minor, d_sharp_minor, a_sharp_minor,
    d_minor, g_minor, c_minor, f_minor,
    b_flat_minor, e_flat_minor, a_flat_minor, 
 ) where


import Mullein.Duration
import Mullein.Core
import Mullein.Pitch

import Data.Ratio

--------------------------------------------------------------------------------
-- Named pitches
-- $pitchdoc 
-- Pre-defined pitches and pitch labels. Middle c is @c4@, octaves start on c. 
-- Sharp and flat notes follow the LilyPond convention with suffix of @is@ for 
-- a sharp and @es@ for a flat.


c_nat     :: PitchLabel
d_nat     :: PitchLabel
e_nat     :: PitchLabel 
f_nat     :: PitchLabel
g_nat     :: PitchLabel
a_nat     :: PitchLabel
b_nat     :: PitchLabel
c_nat     = PitchLabel C Nat
d_nat     = PitchLabel D Nat
e_nat     = PitchLabel E Nat
f_nat     = PitchLabel F Nat
g_nat     = PitchLabel G Nat
a_nat     = PitchLabel A Nat
b_nat     = PitchLabel B Nat

c_sharp   :: PitchLabel
d_sharp   :: PitchLabel
f_sharp   :: PitchLabel 
g_sharp   :: PitchLabel 
a_sharp   :: PitchLabel
c_sharp   = PitchLabel C Sharp
d_sharp   = PitchLabel D Sharp
f_sharp   = PitchLabel F Sharp
g_sharp   = PitchLabel G Sharp
a_sharp   = PitchLabel A Sharp

d_flat    :: PitchLabel
e_flat    :: PitchLabel
g_flat    :: PitchLabel
a_flat    :: PitchLabel
b_flat    :: PitchLabel
d_flat    = PitchLabel D Flat
e_flat    = PitchLabel E Flat
g_flat    = PitchLabel G Flat
a_flat    = PitchLabel A Flat
b_flat    = PitchLabel B Flat

pchNat :: PitchLetter -> Int -> Pitch
pchNat n o    = Pitch n Nat o

pchSharp :: PitchLetter -> Int -> Pitch
pchSharp n o  = Pitch n Sharp o

pchFlat :: PitchLetter -> Int -> Pitch
pchFlat n o   = Pitch n Flat o

middle_c :: Pitch
middle_c = pchNat C 4

c4    :: Pitch
d4    :: Pitch 
e4    :: Pitch
f4    :: Pitch
g4    :: Pitch
a4    :: Pitch 
b4    :: Pitch 
cis4  :: Pitch 
des4  :: Pitch 
dis4  :: Pitch 
ees4  :: Pitch
fis4  :: Pitch
ges4  :: Pitch 
gis4  :: Pitch 
aes4  :: Pitch 
ais4  :: Pitch 
bes4  :: Pitch
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


c3    :: Pitch
d3    :: Pitch 
e3    :: Pitch
f3    :: Pitch
g3    :: Pitch
a3    :: Pitch 
b3    :: Pitch 
cis3  :: Pitch 
des3  :: Pitch 
dis3  :: Pitch 
ees3  :: Pitch
fis3  :: Pitch
ges3  :: Pitch 
gis3  :: Pitch 
aes3  :: Pitch 
ais3  :: Pitch 
bes3  :: Pitch
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

c2    :: Pitch
d2    :: Pitch 
e2    :: Pitch
f2    :: Pitch
g2    :: Pitch
a2    :: Pitch 
b2    :: Pitch 
cis2  :: Pitch 
des2  :: Pitch 
dis2  :: Pitch 
ees2  :: Pitch
fis2  :: Pitch
ges2  :: Pitch 
gis2  :: Pitch 
aes2  :: Pitch 
ais2  :: Pitch 
bes2  :: Pitch
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

c1    :: Pitch
d1    :: Pitch 
e1    :: Pitch
f1    :: Pitch
g1    :: Pitch
a1    :: Pitch 
b1    :: Pitch 
cis1  :: Pitch 
des1  :: Pitch 
dis1  :: Pitch 
ees1  :: Pitch
fis1  :: Pitch
ges1  :: Pitch 
gis1  :: Pitch 
aes1  :: Pitch 
ais1  :: Pitch 
bes1  :: Pitch
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


c5    :: Pitch
d5    :: Pitch 
e5    :: Pitch
f5    :: Pitch
g5    :: Pitch
a5    :: Pitch 
b5    :: Pitch 
cis5  :: Pitch 
des5  :: Pitch 
dis5  :: Pitch 
ees5  :: Pitch
fis5  :: Pitch
ges5  :: Pitch 
gis5  :: Pitch 
aes5  :: Pitch 
ais5  :: Pitch 
bes5  :: Pitch
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

c6    :: Pitch
d6    :: Pitch 
e6    :: Pitch
f6    :: Pitch
g6    :: Pitch
a6    :: Pitch 
b6    :: Pitch 
cis6  :: Pitch 
des6  :: Pitch 
dis6  :: Pitch 
ees6  :: Pitch
fis6  :: Pitch
ges6  :: Pitch 
gis6  :: Pitch 
aes6  :: Pitch 
ais6  :: Pitch 
bes6  :: Pitch
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

c7    :: Pitch
d7    :: Pitch 
e7    :: Pitch
f7    :: Pitch
g7    :: Pitch
a7    :: Pitch 
b7    :: Pitch 
cis7  :: Pitch 
des7  :: Pitch 
dis7  :: Pitch 
ees7  :: Pitch
fis7  :: Pitch
ges7  :: Pitch 
gis7  :: Pitch 
aes7  :: Pitch 
ais7  :: Pitch 
bes7  :: Pitch
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

--------------------------------------------------------------------------------
-- Durations

longa                       :: Duration
longa                       = 4%1

-- $amerdoc
-- American naming.
double_whole                :: Duration
double_whole                = 2%1

whole                       :: Duration
whole                       = 1%1

half                        :: Duration
half                        = 1%2

quarter                     :: Duration
quarter                     = 1%4

eighth                      :: Duration
eighth                      = 1%8

sixteenth                   :: Duration
sixteenth                   = 1%16

thirty_second               :: Duration
thirty_second               = 1%32

sixty_fourth                :: Duration
sixty_fourth                = 1%64

one_hundred_twenty_eighth   :: Duration
one_hundred_twenty_eighth   = 1%128

-- $engdoc
-- English naming.
breve                       :: Duration
breve                       = double_whole

semibreve                   :: Duration
semibreve                   = whole

minim                       :: Duration
minim                       = half

crochet                     :: Duration
crochet                     = quarter

quaver                      :: Duration 
quaver                      = eighth

semiquaver                  :: Duration 
semiquaver                  = sixteenth

demisemiquaver              :: Duration
demisemiquaver              = thirty_second

hemidemisemiquaver          :: Duration 
hemidemisemiquaver          = sixty_fourth

semihemidemisemiquaver      :: Duration
semihemidemisemiquaver      = one_hundred_twenty_eighth

-- Shorthands
du1   :: Duration
du1   = whole

du2   :: Duration
du2   = half

du4   :: Duration
du4   = quarter

du8   :: Duration
du8   = eighth

du16  :: Duration
du16  = sixteenth

du32  :: Duration
du32  = thirty_second

du64  :: Duration
du64  = sixty_fourth

du128 :: Duration
du128 = one_hundred_twenty_eighth

--------------------------------------------------------------------------------
-- keys

majorKey :: PitchLetter -> Accidental -> Key
majorKey l a = Key (PitchLabel l a) Major []
 
c_major :: Key
c_major = majorKey C Nat

g_major :: Key
g_major = majorKey G Nat

d_major :: Key
d_major = majorKey D Nat

a_major :: Key
a_major = majorKey A Nat

e_major :: Key
e_major = majorKey E Nat

b_major :: Key
b_major = majorKey B Nat

f_sharp_major :: Key
f_sharp_major = majorKey F Sharp

c_sharp_major :: Key
c_sharp_major = majorKey C Sharp

f_major :: Key
f_major = majorKey F Nat

b_flat_major :: Key
b_flat_major = majorKey B Flat

e_flat_major :: Key
e_flat_major = majorKey E Flat

a_flat_major :: Key
a_flat_major = majorKey A Flat

d_flat_major :: Key
d_flat_major = majorKey D Flat

g_flat_major :: Key
g_flat_major = majorKey G Flat

c_flat_major :: Key
c_flat_major = majorKey C Flat



minorKey :: PitchLetter -> Accidental -> Key
minorKey l a = Key (PitchLabel l a) Minor []

a_minor :: Key
a_minor = minorKey A Nat

e_minor :: Key
e_minor = minorKey E Nat

b_minor :: Key
b_minor = minorKey B Nat
  
f_sharp_minor :: Key
f_sharp_minor = minorKey F Sharp

c_sharp_minor :: Key
c_sharp_minor = minorKey C Sharp

g_sharp_minor :: Key
g_sharp_minor = minorKey G Sharp

d_sharp_minor :: Key
d_sharp_minor = minorKey D Sharp

a_sharp_minor :: Key
a_sharp_minor = minorKey A Sharp

d_minor :: Key
d_minor = minorKey D Nat

g_minor :: Key
g_minor = minorKey G Nat

c_minor :: Key
c_minor = minorKey C Nat

f_minor :: Key
f_minor = minorKey F Nat

b_flat_minor :: Key
b_flat_minor = minorKey B Flat

e_flat_minor :: Key
e_flat_minor = minorKey E Flat

a_flat_minor :: Key
a_flat_minor = minorKey A Flat


