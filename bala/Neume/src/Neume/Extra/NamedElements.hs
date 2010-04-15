{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.NamedElements
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Named musical elements e.g. notes, rests, LilyPond drum pitches...
--
--------------------------------------------------------------------------------

module Neume.Extra.NamedElements 
  (
  
  -- * Metrical specs
    four_four_time
  , two_four_time
    
  -- * Note construction functions
  , cf_, c_, cs_, df_, d_, ds_, ef_, e_, es_
  , ff_, f_, fs_, gf_, g_, gs_, af_, a_, as_
  , bf_, b_, bs_

  , wn,  hn,  qn,  en, sn, tn
  , dhn, dqn, den, dsn
    
  -- * Rests
  , wnr, hnr, qnr, enr, snr, tnr
  , dhnr, dqnr, denr, dsnr



  -- * Named pitches
  -- $pitchdoc 
  , c_nat, d_nat, e_nat, f_nat, g_nat, a_nat, b_nat
  , c_sharp, d_sharp, f_sharp, g_sharp, a_sharp
  , d_flat, e_flat, g_flat, a_flat, b_flat



 ) where


import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.Pitch
import Neume.Core.SyntaxGlyph




--------------------------------------------------------------------------------
-- Metrical specs

-- | 4/4 time.
four_four_time  :: MeterPattern
four_four_time  = makeMeterPattern 4 4

-- | 2/4 time.
two_four_time   :: MeterPattern
two_four_time   = makeMeterPattern 2 4




--------------------------------------------------------------------------------
-- notes

cf_       :: Octave -> Note () Pitch
cf_       = Note () . Pitch C (Just Flat)

c_        :: Octave -> Note () Pitch
c_        = Note () . Pitch C Nothing

cs_       :: Octave -> Note () Pitch
cs_       = Note () . Pitch C (Just Sharp)

df_       :: Octave -> Note () Pitch
df_       = Note () . Pitch D (Just Flat)

d_        :: Octave -> Note () Pitch
d_        = Note () . Pitch D Nothing

ds_       :: Octave -> Note () Pitch
ds_       = Note () . Pitch D (Just Sharp)

ef_       :: Octave -> Note () Pitch
ef_       = Note () . Pitch E (Just Flat)

e_        :: Octave -> Note () Pitch
e_        = Note () . Pitch E Nothing 

es_       :: Octave -> Note () Pitch
es_       = Note () . Pitch E (Just Sharp)

ff_       :: Octave -> Note () Pitch
ff_       = Note () . Pitch F (Just Flat)

f_        :: Octave -> Note () Pitch
f_        = Note () . Pitch F Nothing

fs_       :: Octave -> Note () Pitch
fs_       = Note () . Pitch F (Just Sharp)

gf_       :: Octave -> Note () Pitch
gf_       = Note () . Pitch G (Just Flat)

g_        :: Octave -> Note () Pitch
g_        = Note () . Pitch G Nothing

gs_       :: Octave -> Note () Pitch
gs_       = Note () . Pitch G (Just Sharp)

af_       :: Octave -> Note () Pitch
af_       = Note () . Pitch A (Just Flat)

a_        :: Octave -> Note () Pitch
a_        = Note () . Pitch A Nothing

as_       :: Octave -> Note () Pitch
as_       = Note () . Pitch A (Just Sharp)

bf_       :: Octave -> Note () Pitch
bf_       = Note () . Pitch B (Just Flat)

b_        :: Octave -> Note () Pitch
b_        = Note () . Pitch B Nothing

bs_       :: Octave -> Note () Pitch
bs_       = Note () . Pitch B (Just Sharp)


liftNote :: drn -> Note anno pch -> Glyph anno pch drn
liftNote drn nt = GlyNote nt drn False

-- durational /lifters/

wn        :: Note anno pch -> Glyph anno pch Duration
wn        = liftNote dWhole

hn        :: Note anno pch -> Glyph anno pch Duration
hn        = liftNote dHalf

qn        :: Note anno pch -> Glyph anno pch Duration
qn        = liftNote dQuarter

en        :: Note anno pch -> Glyph anno pch Duration
en        = liftNote dEighth

sn        :: Note anno pch -> Glyph anno pch Duration
sn        = liftNote dSixteenth

tn        :: Note anno pch -> Glyph anno pch Duration
tn        = liftNote dThirtySecondth

dhn       :: Note anno pch -> Glyph anno pch Duration
dhn       = liftNote $ dot dHalf

dqn       :: Note anno pch -> Glyph anno pch Duration
dqn       = liftNote $ dot dQuarter

den       :: Note anno pch -> Glyph anno pch Duration
den       = liftNote $ dot dEighth

dsn       :: Note anno pch -> Glyph anno pch Duration
dsn       = liftNote $ dot dSixteenth


-- durations

-- rests

wnr     :: MakeRest e => e
wnr     = makeRest dWhole

hnr     :: MakeRest e => e
hnr     = makeRest dHalf

qnr     :: MakeRest e => e
qnr     = makeRest dQuarter

enr     :: MakeRest e => e
enr     = makeRest dEighth

snr     :: MakeRest e => e
snr     = makeRest dSixteenth

tnr     :: MakeRest e => e
tnr     = makeRest dThirtySecondth

dhnr    :: MakeRest e => e
dhnr    = makeRest $ dot dHalf

dqnr    :: MakeRest e => e
dqnr    = makeRest $ dot dQuarter

denr    :: MakeRest e => e
denr    = makeRest $ dot dEighth

dsnr    :: MakeRest e => e
dsnr    = makeRest $ dot dSixteenth


--------------------------------------------------------------------------------
-- Named pitches
-- $pitchdoc 
-- Pre-defined pitches and pitch labels. Middle c is @c4@, octaves start on c. 


c_nat     :: PitchLabel
c_nat     = PitchLabel C Nothing

d_nat     :: PitchLabel
d_nat     = PitchLabel D Nothing

e_nat     :: PitchLabel 
e_nat     = PitchLabel E Nothing

f_nat     :: PitchLabel
f_nat     = PitchLabel F Nothing

g_nat     :: PitchLabel
g_nat     = PitchLabel G Nothing

a_nat     :: PitchLabel
a_nat     = PitchLabel A Nothing

b_nat     :: PitchLabel
b_nat     = PitchLabel B Nothing


c_sharp   :: PitchLabel
c_sharp   = PitchLabel C (Just Sharp)

d_sharp   :: PitchLabel
d_sharp   = PitchLabel D (Just Sharp)

f_sharp   :: PitchLabel 
f_sharp   = PitchLabel F (Just Sharp)

g_sharp   :: PitchLabel 
g_sharp   = PitchLabel G (Just Sharp)

a_sharp   :: PitchLabel
a_sharp   = PitchLabel A (Just Sharp)


d_flat    :: PitchLabel
d_flat    = PitchLabel D (Just Flat)

e_flat    :: PitchLabel
e_flat    = PitchLabel E (Just Flat)

g_flat    :: PitchLabel
g_flat    = PitchLabel G (Just Flat)

a_flat    :: PitchLabel
a_flat    = PitchLabel A (Just Flat)

b_flat    :: PitchLabel
b_flat    = PitchLabel B (Just Flat)
