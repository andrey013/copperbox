{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.NamedElements
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Named musical elements e.g. notes, keys...
--
--------------------------------------------------------------------------------

module Mullein.NamedElements 
  (

    
  -- note functions
    cf, c, cs, df, d, ds, ef, e, es
  , ff, f, fs, gf, g, gs, af, a, as
  , bf, b, bs

    
    -- rests
  , wnr, hnr, qnr, enr, snr, tnr
  , dhnr, dqnr, denr, dsnr



  -- * Named pitches
  -- $pitchdoc 
  , c_nat, d_nat, e_nat, f_nat, g_nat, a_nat, b_nat
  , c_sharp, d_sharp, f_sharp, g_sharp, a_sharp
  , d_flat, e_flat, g_flat, a_flat, b_flat


  , middle_c
        
  , c5, d5, e5, f5, g5, a5, b5
  , cs5, df5, ds5, ef5, fs5, gf5, gs5, af5, as5, bf5

  , c4, d4, e4, f4, g4, a4, b4
  , cs4, df4, ds4, ef4, fs4, gf4, gs4, af4, as4, bf4
    
  , c3, d3, e3, f3, g3, a3, b3
  , cs3, df3, ds3, ef3, fs3, gf3, gs3, af3, as3, bf3
  
  , c2, d2, e2, f2, g2, a2, b2
  , cs2, df2, ds2, ef2, fs2, gf2, gs2, af2, as2, bf2
  
  , c1, d1, e1, f1, g1, a1, b1
  , cs1, df1, ds1, ef1, fs1, gf1, gs1, af1, as1, bf1
    
  , c6, d6, e6, f6, g6, a6, b6
  , cs6, df6, ds6, ef6, fs6, gf6, gs6, af6, as6, bf6
  
  , c7, d7, e7, f7, g7, a7, b7
  , cs7, df7, ds7, ef7, fs7, gf7, gs7, af7, as7, bf7


 ) where


import Mullein.Duration
import Mullein.Core
import Mullein.Pitch





-- notes
cf :: Octave -> Duration -> Glyph Duration Pitch
c  :: Octave -> Duration -> Glyph Duration Pitch
cs :: Octave -> Duration -> Glyph Duration Pitch
df :: Octave -> Duration -> Glyph Duration Pitch
d  :: Octave -> Duration -> Glyph Duration Pitch
ds :: Octave -> Duration -> Glyph Duration Pitch
ef :: Octave -> Duration -> Glyph Duration Pitch
e  :: Octave -> Duration -> Glyph Duration Pitch
es :: Octave -> Duration -> Glyph Duration Pitch
ff :: Octave -> Duration -> Glyph Duration Pitch
f  :: Octave -> Duration -> Glyph Duration Pitch
fs :: Octave -> Duration -> Glyph Duration Pitch
gf :: Octave -> Duration -> Glyph Duration Pitch
g  :: Octave -> Duration -> Glyph Duration Pitch
gs :: Octave -> Duration -> Glyph Duration Pitch
af :: Octave -> Duration -> Glyph Duration Pitch
a  :: Octave -> Duration -> Glyph Duration Pitch
as :: Octave -> Duration -> Glyph Duration Pitch
bf :: Octave -> Duration -> Glyph Duration Pitch
b  :: Octave -> Duration -> Glyph Duration Pitch
bs :: Octave -> Duration -> Glyph Duration Pitch


cf o dur = Note dur (Pitch C (Just Flat) o)
c  o dur = Note dur (Pitch C Nothing o)
cs o dur = Note dur (Pitch C (Just Sharp) o)
df o dur = Note dur (Pitch D (Just Flat) o)
d  o dur = Note dur (Pitch D Nothing o)
ds o dur = Note dur (Pitch D (Just Sharp) o)
ef o dur = Note dur (Pitch E (Just Flat) o)
e  o dur = Note dur (Pitch E Nothing o)
es o dur = Note dur (Pitch E (Just Sharp) o)
ff o dur = Note dur (Pitch F (Just Flat) o)
f  o dur = Note dur (Pitch F Nothing o)
fs o dur = Note dur (Pitch F (Just Sharp) o)
gf o dur = Note dur (Pitch G (Just Flat) o)
g  o dur = Note dur (Pitch G Nothing o)
gs o dur = Note dur (Pitch G (Just Sharp) o)
af o dur = Note dur (Pitch A (Just Flat) o)
a  o dur = Note dur (Pitch A Nothing o)
as o dur = Note dur (Pitch A (Just Sharp) o)
bf o dur = Note dur (Pitch B (Just Flat) o)
b  o dur = Note dur (Pitch B Nothing o)
bs o dur = Note dur (Pitch B (Just Sharp) o)



-- durations

-- rests

wnr :: Glyph Duration Pitch
hnr :: Glyph Duration Pitch
qnr :: Glyph Duration Pitch
enr :: Glyph Duration Pitch
snr :: Glyph Duration Pitch
tnr :: Glyph Duration Pitch

wnr = Rest wn
hnr = Rest hn
qnr = Rest qn
enr = Rest en
snr = Rest sn
tnr = Rest tn

dhnr :: Glyph Duration Pitch
dqnr :: Glyph Duration Pitch
denr :: Glyph Duration Pitch
dsnr :: Glyph Duration Pitch

dhnr = Rest dhn
dqnr = Rest dqn
denr = Rest den
dsnr = Rest dsn


--------------------------------------------------------------------------------
-- Named pitches
-- $pitchdoc 
-- Pre-defined pitches and pitch labels. Middle c is @c4@, octaves start on c. 


c_nat     :: PitchLabel
d_nat     :: PitchLabel
e_nat     :: PitchLabel 
f_nat     :: PitchLabel
g_nat     :: PitchLabel
a_nat     :: PitchLabel
b_nat     :: PitchLabel
c_nat     = PitchLabel C Nothing
d_nat     = PitchLabel D Nothing
e_nat     = PitchLabel E Nothing
f_nat     = PitchLabel F Nothing
g_nat     = PitchLabel G Nothing
a_nat     = PitchLabel A Nothing
b_nat     = PitchLabel B Nothing

c_sharp   :: PitchLabel
d_sharp   :: PitchLabel
f_sharp   :: PitchLabel 
g_sharp   :: PitchLabel 
a_sharp   :: PitchLabel
c_sharp   = PitchLabel C (Just Sharp)
d_sharp   = PitchLabel D (Just Sharp)
f_sharp   = PitchLabel F (Just Sharp)
g_sharp   = PitchLabel G (Just Sharp)
a_sharp   = PitchLabel A (Just Sharp)

d_flat    :: PitchLabel
e_flat    :: PitchLabel
g_flat    :: PitchLabel
a_flat    :: PitchLabel
b_flat    :: PitchLabel
d_flat    = PitchLabel D (Just Flat)
e_flat    = PitchLabel E (Just Flat)
g_flat    = PitchLabel G (Just Flat)
a_flat    = PitchLabel A (Just Flat)
b_flat    = PitchLabel B (Just Flat)

pchNat :: PitchLetter -> Int -> Pitch
pchNat n o    = Pitch n Nothing o

pchSharp :: PitchLetter -> Int -> Pitch
pchSharp n o  = Pitch n (Just Sharp) o

pchFlat :: PitchLetter -> Int -> Pitch
pchFlat n o   = Pitch n (Just Flat) o

middle_c :: Pitch
middle_c = pchNat C 5


c5        :: Pitch
d5        :: Pitch 
e5        :: Pitch
f5        :: Pitch
g5        :: Pitch
a5        :: Pitch 
b5        :: Pitch 
cs5       :: Pitch 
df5       :: Pitch 
ds5       :: Pitch 
ef5       :: Pitch
fs5       :: Pitch
gf5       :: Pitch 
gs5       :: Pitch 
af5       :: Pitch 
as5       :: Pitch 
bf5       :: Pitch
c5        = pchNat C 5
d5        = pchNat D 5
e5        = pchNat E 5
f5        = pchNat F 5
g5        = pchNat G 5
a5        = pchNat A 5
b5        = pchNat B 5
cs5       = pchSharp C 5
df5       = pchFlat D 5
ds5       = pchSharp D 5
ef5       = pchFlat E 5
fs5       = pchSharp F 5
gf5       = pchFlat G 5
gs5       = pchSharp G 5
af5       = pchFlat A 5
as5       = pchSharp A 5
bf5       = pchFlat B 5


c4        :: Pitch
d4        :: Pitch 
e4        :: Pitch
f4        :: Pitch
g4        :: Pitch
a4        :: Pitch 
b4        :: Pitch 
cs4       :: Pitch 
df4       :: Pitch 
ds4       :: Pitch 
ef4       :: Pitch
fs4       :: Pitch
gf4       :: Pitch 
gs4       :: Pitch 
af4       :: Pitch 
as4       :: Pitch 
bf4       :: Pitch
c4        = pchNat C 4
d4        = pchNat D 4
e4        = pchNat E 4
f4        = pchNat F 4
g4        = pchNat G 4
a4        = pchNat A 4
b4        = pchNat B 4
cs4       = pchSharp C 4
df4       = pchFlat D 4
ds4       = pchSharp D 4
ef4       = pchFlat E 4
fs4       = pchSharp F 4
gf4       = pchFlat G 4
gs4       = pchSharp G 4
af4       = pchFlat A 4
as4       = pchSharp A 4
bf4       = pchFlat B 4


c3        :: Pitch
d3        :: Pitch 
e3        :: Pitch
f3        :: Pitch
g3        :: Pitch
a3        :: Pitch 
b3        :: Pitch 
cs3       :: Pitch 
df3       :: Pitch 
ds3       :: Pitch 
ef3       :: Pitch
fs3       :: Pitch
gf3       :: Pitch 
gs3       :: Pitch 
af3       :: Pitch 
as3       :: Pitch 
bf3       :: Pitch
c3        = pchNat C 3
d3        = pchNat D 3
e3        = pchNat E 3
f3        = pchNat F 3
g3        = pchNat G 3
a3        = pchNat A 3
b3        = pchNat B 3
cs3       = pchSharp C 3
df3       = pchFlat D 3
ds3       = pchSharp D 3
ef3       = pchFlat E 3
fs3       = pchSharp F 3
gf3       = pchFlat G 3
gs3       = pchSharp G 3
af3       = pchFlat A 3
as3       = pchSharp A 3
bf3       = pchFlat B 3

c2        :: Pitch
d2        :: Pitch 
e2        :: Pitch
f2        :: Pitch
g2        :: Pitch
a2        :: Pitch 
b2        :: Pitch 
cs2       :: Pitch 
df2       :: Pitch 
ds2       :: Pitch 
ef2       :: Pitch
fs2       :: Pitch
gf2       :: Pitch 
gs2       :: Pitch 
af2       :: Pitch 
as2       :: Pitch 
bf2       :: Pitch
c2        = pchNat C 2
d2        = pchNat D 2
e2        = pchNat E 2
f2        = pchNat F 2
g2        = pchNat G 2
a2        = pchNat A 2
b2        = pchNat B 2
cs2       = pchSharp C 2
df2       = pchFlat D 2
ds2       = pchSharp D 2
ef2       = pchFlat E 2
fs2       = pchSharp F 2
gf2       = pchFlat G 2
gs2       = pchSharp G 2
af2       = pchFlat A 2
as2       = pchSharp A 2
bf2       = pchFlat B 2

c1        :: Pitch
d1        :: Pitch 
e1        :: Pitch
f1        :: Pitch
g1        :: Pitch
a1        :: Pitch 
b1        :: Pitch 
cs1       :: Pitch 
df1       :: Pitch 
ds1       :: Pitch 
ef1       :: Pitch
fs1       :: Pitch
gf1       :: Pitch 
gs1       :: Pitch 
af1       :: Pitch 
as1       :: Pitch 
bf1       :: Pitch
c1        = pchNat C 1
d1        = pchNat D 1
e1        = pchNat E 1
f1        = pchNat F 1
g1        = pchNat G 1
a1        = pchNat A 1
b1        = pchNat B 1
cs1       = pchSharp C 1
df1       = pchFlat D 1
ds1       = pchSharp D 1
ef1       = pchFlat E 1
fs1       = pchSharp F 1
gf1       = pchFlat G 1
gs1       = pchSharp G 1
af1       = pchFlat A 1
as1       = pchSharp A 1
bf1       = pchFlat B 1


c6        :: Pitch
d6        :: Pitch 
e6        :: Pitch
f6        :: Pitch
g6        :: Pitch
a6        :: Pitch 
b6        :: Pitch 
cs6       :: Pitch 
df6       :: Pitch 
ds6       :: Pitch 
ef6       :: Pitch
fs6       :: Pitch
gf6       :: Pitch 
gs6       :: Pitch 
af6       :: Pitch 
as6       :: Pitch 
bf6       :: Pitch
c6        = pchNat C 6
d6        = pchNat D 6
e6        = pchNat E 6
f6        = pchNat F 6
g6        = pchNat G 6
a6        = pchNat A 6
b6        = pchNat B 6
cs6       = pchSharp C 6
df6       = pchFlat D 6
ds6       = pchSharp D 6
ef6       = pchFlat E 6
fs6       = pchSharp F 6
gf6       = pchFlat G 6
gs6       = pchSharp G 6
af6       = pchFlat A 6
as6       = pchSharp A 6
bf6       = pchFlat B 6

c7        :: Pitch
d7        :: Pitch 
e7        :: Pitch
f7        :: Pitch
g7        :: Pitch
a7        :: Pitch 
b7        :: Pitch 
cs7       :: Pitch 
df7       :: Pitch 
ds7       :: Pitch 
ef7       :: Pitch
fs7       :: Pitch
gf7       :: Pitch 
gs7       :: Pitch 
af7       :: Pitch 
as7       :: Pitch 
bf7       :: Pitch
c7        = pchNat C 7
d7        = pchNat D 7
e7        = pchNat E 7
f7        = pchNat F 7
g7        = pchNat G 7
a7        = pchNat A 7
b7        = pchNat B 7
cs7       = pchSharp C 7
df7       = pchFlat D 7
ds7       = pchSharp D 7
ef7       = pchFlat E 7
fs7       = pchSharp F 7
gf7       = pchFlat G 7
gs7       = pchSharp G 7
af7       = pchFlat A 7
as7       = pchSharp A 7
bf7       = pchFlat B 7

