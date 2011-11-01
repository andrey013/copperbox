{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.NamedElements
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Short hand constructors for pitch labels...
--
--------------------------------------------------------------------------------

module Neume.Core.NamedElements 
  (

  -- * Note construction functions
    cf_, c_, cs_, df_, d_, ds_, ef_, e_, es_
  , ff_, f_, fs_, gf_, g_, gs_, af_, a_, as_
  , bf_, b_, bs_
  

  , wn,  hn,  qn,  en, sn, tn
  , dhn, dqn, den, dsn

  -- * Named pitch labels
 
  , c_nat, d_nat, e_nat, f_nat, g_nat, a_nat, b_nat
  , c_sharp, d_sharp, f_sharp, g_sharp, a_sharp
  , d_flat, e_flat, g_flat, a_flat, b_flat



 ) where


import Neume.Core.Duration
import Neume.Core.LilyPondMonad
import Neume.Core.Pitch






--------------------------------------------------------------------------------
-- notes

cf_       :: Octave -> Pitch
cf_       = Pitch C (Just Flat)

c_        :: Octave -> Pitch
c_        = Pitch C Nothing

cs_       :: Octave -> Pitch
cs_       = Pitch C (Just Sharp)

df_       :: Octave -> Pitch
df_       = Pitch D (Just Flat)

d_        :: Octave -> Pitch
d_        = Pitch D Nothing

ds_       :: Octave -> Pitch
ds_       = Pitch D (Just Sharp)

ef_       :: Octave -> Pitch
ef_       = Pitch E (Just Flat)

e_        :: Octave -> Pitch
e_        = Pitch E Nothing 

es_       :: Octave -> Pitch
es_       = Pitch E (Just Sharp)

ff_       :: Octave -> Pitch
ff_       = Pitch F (Just Flat)

f_        :: Octave -> Pitch
f_        = Pitch F Nothing

fs_       :: Octave -> Pitch
fs_       = Pitch F (Just Sharp)

gf_       :: Octave -> Pitch
gf_       = Pitch G (Just Flat)

g_        :: Octave -> Pitch
g_        = Pitch G Nothing

gs_       :: Octave -> Pitch
gs_       = Pitch G (Just Sharp)

af_       :: Octave -> Pitch
af_       = Pitch A (Just Flat)

a_        :: Octave -> Pitch
a_        = Pitch A Nothing

as_       :: Octave -> Pitch
as_       = Pitch A (Just Sharp)

bf_       :: Octave -> Pitch
bf_       = Pitch B (Just Flat)

b_        :: Octave -> Pitch
b_        = Pitch B Nothing

bs_       :: Octave -> Pitch
bs_       = Pitch B (Just Sharp)




wn        :: Pitch -> LyNoteListM ()
wn        = note `flip` dWhole

hn        :: Pitch -> LyNoteListM ()
hn        = note `flip` dHalf

qn        :: Pitch -> LyNoteListM ()
qn        = note `flip` dQuarter

en        :: Pitch -> LyNoteListM ()
en        = note `flip` dEighth

sn        :: Pitch -> LyNoteListM ()
sn        = note `flip` dSixteenth

tn        :: Pitch -> LyNoteListM ()
tn        = note `flip` dThirtySecondth

dhn       :: Pitch -> LyNoteListM ()
dhn       = note `flip` (dot dHalf)

dqn       :: Pitch -> LyNoteListM ()
dqn       = note `flip` (dot dQuarter)

den       :: Pitch -> LyNoteListM ()
den       = note `flip` (dot dEighth)

dsn       :: Pitch -> LyNoteListM ()
dsn       = note `flip` (dot dSixteenth)

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
