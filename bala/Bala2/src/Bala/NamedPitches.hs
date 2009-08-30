{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.NamedPitches
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Named pitches.
--
--------------------------------------------------------------------------------

module Bala.NamedPitches
  ( 
    c5, d5, e5, f5, g5, a5, b5
  , cs5, df5, ds5, ef5, fs5, gf5, gs5, af5, as5, bf5

  , c1, d1, e1, f1, g1, a1, b1
  , cs1, df1, ds1, ef1, fs1, gf1, gs1, af1, as1, bf1

  , c2, d2, e2, f2, g2, a2, b2
  , cs2, df2, ds2, ef2, fs2, gf2, gs2, af2, as2, bf2

  , c3, d3, e3, f3, g3, a3, b3
  , cs3, df3, ds3, ef3, fs3, gf3, gs3, af3, as3, bf3

  , c4, d4, e4, f4, g4, a4, b4
  , cs4, df4, ds4, ef4, fs4, gf4, gs4, af4, as4, bf4

  , c6, d6, e6, f6, g6, a6, b6
  , cs6, df6, ds6, ef6, fs6, gf6, gs6, af6, as6, bf6

  , c7, d7, e7, f7, g7, a7, b7
  , cs7, df7, ds7, ef7, fs7, gf7, gs7, af7, as7, bf7

  , c8, d8, e8, f8, g8, a8, b8
  , cs8, df8, ds8, ef8, fs8, gf8, gs8, af8, as8, bf8


  ) where


import Bala.Pitch 


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
c5        = Pitch C 0 5
d5        = Pitch D 0 5
e5        = Pitch E 0 5
f5        = Pitch F 0 5
g5        = Pitch G 0 5
a5        = Pitch A 0 5
b5        = Pitch B 0 5
cs5       = Pitch C 1 5
df5       = Pitch D (-1) 5
ds5       = Pitch D 1 5
ef5       = Pitch E (-1) 5
fs5       = Pitch F 1 5
gf5       = Pitch G (-1) 5
gs5       = Pitch G 1 5
af5       = Pitch A (-1) 5
as5       = Pitch A 1 5
bf5       = Pitch B (-1) 5

--------------------------------------------------------------------------------

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
c1        = Pitch C 0 1
d1        = Pitch D 0 1
e1        = Pitch E 0 1
f1        = Pitch F 0 1
g1        = Pitch G 0 1
a1        = Pitch A 0 1
b1        = Pitch B 0 1
cs1       = Pitch C 1 1
df1       = Pitch D (-1) 1
ds1       = Pitch D 1 1
ef1       = Pitch E (-1) 1
fs1       = Pitch F 1 1
gf1       = Pitch G (-1) 1
gs1       = Pitch G 1 1
af1       = Pitch A (-1) 1
as1       = Pitch A 1 1
bf1       = Pitch B (-1) 1



--------------------------------------------------------------------------------

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
c2        = Pitch C 0 2
d2        = Pitch D 0 2
e2        = Pitch E 0 2
f2        = Pitch F 0 2
g2        = Pitch G 0 2
a2        = Pitch A 0 2
b2        = Pitch B 0 2
cs2       = Pitch C 1 2
df2       = Pitch D (-1) 2
ds2       = Pitch D 1 2
ef2       = Pitch E (-1) 2
fs2       = Pitch F 1 2
gf2       = Pitch G (-1) 2
gs2       = Pitch G 1 2
af2       = Pitch A (-1) 2
as2       = Pitch A 1 2
bf2       = Pitch B (-1) 2


--------------------------------------------------------------------------------

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
c3        = Pitch C 0 3
d3        = Pitch D 0 3
e3        = Pitch E 0 3
f3        = Pitch F 0 3
g3        = Pitch G 0 3
a3        = Pitch A 0 3
b3        = Pitch B 0 3
cs3       = Pitch C 1 3
df3       = Pitch D (-1) 3
ds3       = Pitch D 1 3
ef3       = Pitch E (-1) 3
fs3       = Pitch F 1 3
gf3       = Pitch G (-1) 3
gs3       = Pitch G 1 3
af3       = Pitch A (-1) 3
as3       = Pitch A 1 3
bf3       = Pitch B (-1) 3


--------------------------------------------------------------------------------

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
c4        = Pitch C 0 4
d4        = Pitch D 0 4
e4        = Pitch E 0 4
f4        = Pitch F 0 4
g4        = Pitch G 0 4
a4        = Pitch A 0 4
b4        = Pitch B 0 4
cs4       = Pitch C 1 4
df4       = Pitch D (-1) 4
ds4       = Pitch D 1 4
ef4       = Pitch E (-1) 4
fs4       = Pitch F 1 4
gf4       = Pitch G (-1) 4
gs4       = Pitch G 1 4
af4       = Pitch A (-1) 4
as4       = Pitch A 1 4
bf4       = Pitch B (-1) 4


--------------------------------------------------------------------------------

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
c6        = Pitch C 0 6
d6        = Pitch D 0 6
e6        = Pitch E 0 6
f6        = Pitch F 0 6
g6        = Pitch G 0 6
a6        = Pitch A 0 6
b6        = Pitch B 0 6
cs6       = Pitch C 1 6
df6       = Pitch D (-1) 6
ds6       = Pitch D 1 6
ef6       = Pitch E (-1) 6
fs6       = Pitch F 1 6
gf6       = Pitch G (-1) 6
gs6       = Pitch G 1 6
af6       = Pitch A (-1) 6
as6       = Pitch A 1 6
bf6       = Pitch B (-1) 6

--------------------------------------------------------------------------------

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
c7        = Pitch C 0 7
d7        = Pitch D 0 7
e7        = Pitch E 0 7
f7        = Pitch F 0 7
g7        = Pitch G 0 7
a7        = Pitch A 0 7
b7        = Pitch B 0 7
cs7       = Pitch C 1 7
df7       = Pitch D (-1) 7
ds7       = Pitch D 1 7
ef7       = Pitch E (-1) 7
fs7       = Pitch F 1 7
gf7       = Pitch G (-1) 7
gs7       = Pitch G 1 7
af7       = Pitch A (-1) 7
as7       = Pitch A 1 7
bf7       = Pitch B (-1) 7

--------------------------------------------------------------------------------

c8        :: Pitch
d8        :: Pitch 
e8        :: Pitch
f8        :: Pitch
g8        :: Pitch
a8        :: Pitch 
b8        :: Pitch 
cs8       :: Pitch 
df8       :: Pitch 
ds8       :: Pitch 
ef8       :: Pitch
fs8       :: Pitch
gf8       :: Pitch 
gs8       :: Pitch 
af8       :: Pitch 
as8       :: Pitch 
bf8       :: Pitch
c8        = Pitch C 0 8
d8        = Pitch D 0 8
e8        = Pitch E 0 8
f8        = Pitch F 0 8
g8        = Pitch G 0 8
a8        = Pitch A 0 8
b8        = Pitch B 0 8
cs8       = Pitch C 1 8
df8       = Pitch D (-1) 8
ds8       = Pitch D 1 8
ef8       = Pitch E (-1) 8
fs8       = Pitch F 1 8
gf8       = Pitch G (-1) 8
gs8       = Pitch G 1 8
af8       = Pitch A (-1) 8
as8       = Pitch A 1 8
bf8       = Pitch B (-1) 8

