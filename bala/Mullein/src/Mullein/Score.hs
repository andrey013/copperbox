{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Score
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Named musical elements (e.g. notes, keys) within the NoteCtx monad
--
--------------------------------------------------------------------------------

module Mullein.Score (

    part,

    phrase, repeated, fsrepeat,
    
    motif,
    primary, addOverlay,
--    notelist,

    rest, space, note, chord,
    (%%),    
    
    -- notes
    cf, c, cs, df, d, ds, ef, e, es,
    ff, f, fs, gf, g, gs, af, a, as,
    bf, b, bs,

    -- durations
    wn, hn, qn, en, sn, tn,
    dhn, dqn, den, dsn,
    
    -- rests
    wnr, hnr, qnr, enr, snr, tnr,
    dhnr, dqnr, denr, dsnr,


 ) where

import Mullein.Bracket
import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch

import Data.Ratio


part :: [PhraseP e] -> PartP e
part = Part

phrase :: MotifP e -> PhraseP e
phrase  = Phrase

repeated :: MotifP e -> PhraseP e
repeated = Repeated

fsrepeat :: MotifP e
         -> MotifP e
         -> MotifP e
         -> PhraseP e
fsrepeat = FSRepeat

motif :: Key -> MetricalSpec -> OverlayList e -> MotifP e
motif = bracket


primary :: [ElementP e] -> OverlayList e
primary xs = (xs,[])

addOverlay :: BarNum -> [ElementP e] -> (OverlayList e)
           -> (OverlayList e)
addOverlay n xs (p,xss) = (p,(n,xs):xss)


-- notelist :: [e] -> NoteCtx [e]
-- notelist ss = return ss 

--


rest :: Duration -> ElementP e
rest = Rest

space :: Duration -> ElementP e
space = Spacer


note :: Pitch -> Duration -> Element
note pch dur   = Note pch dur

chord :: [Pitch] -> Duration -> Element
chord xs dur = Chord xs dur


-- alternative to @note@ with more general type
(%%) :: e -> Duration -> ElementP e
(%%) pch dur = Note pch dur

-- notes
cf :: Octave -> Duration -> Element
c  :: Octave -> Duration -> Element
cs :: Octave -> Duration -> Element
df :: Octave -> Duration -> Element
d  :: Octave -> Duration -> Element
ds :: Octave -> Duration -> Element
ef :: Octave -> Duration -> Element
e  :: Octave -> Duration -> Element
es :: Octave -> Duration -> Element
ff :: Octave -> Duration -> Element
f  :: Octave -> Duration -> Element
fs :: Octave -> Duration -> Element
gf :: Octave -> Duration -> Element
g  :: Octave -> Duration -> Element
gs :: Octave -> Duration -> Element
af :: Octave -> Duration -> Element
a  :: Octave -> Duration -> Element
as :: Octave -> Duration -> Element
bf :: Octave -> Duration -> Element
b  :: Octave -> Duration -> Element
bs :: Octave -> Duration -> Element


cf o = Note (Pitch C Flat o)
c  o = Note (Pitch C Nat o)
cs o = Note (Pitch C Sharp o)
df o = Note (Pitch D Flat o)
d  o = Note (Pitch D Nat o)
ds o = Note (Pitch D Sharp o)
ef o = Note (Pitch E Flat o)
e  o = Note (Pitch E Nat o)
es o = Note (Pitch E Sharp o)
ff o = Note (Pitch F Flat o)
f  o = Note (Pitch F Nat o)
fs o = Note (Pitch F Sharp o)
gf o = Note (Pitch G Flat o)
g  o = Note (Pitch G Nat o)
gs o = Note (Pitch G Sharp o)
af o = Note (Pitch A Flat o)
a  o = Note (Pitch A Nat o)
as o = Note (Pitch A Sharp o)
bf o = Note (Pitch B Flat o)
b  o = Note (Pitch B Nat o)
bs o = Note (Pitch B Sharp o)



-- durations

wn :: Duration

hn :: Duration
qn :: Duration
en :: Duration
sn :: Duration
tn :: Duration

wn = 1
hn = 1%2
qn = 1%4
en = 1%8
sn = 1%16
tn = 1%32

dhn :: Duration
dqn :: Duration
den :: Duration
dsn :: Duration

dhn = 3%4
dqn = 3%8
den = 3%16
dsn = 3%32

-- rests

wnr :: ElementP e
hnr :: ElementP e
qnr :: ElementP e
enr :: ElementP e
snr :: ElementP e
tnr :: ElementP e

wnr = Rest wn
hnr = Rest hn
qnr = Rest qn
enr = Rest en
snr = Rest sn
tnr = Rest tn

dhnr :: ElementP e
dqnr :: ElementP e
denr :: ElementP e
dsnr :: ElementP e

dhnr = Rest dhn
dqnr = Rest dqn
denr = Rest den
dsnr = Rest dsn



