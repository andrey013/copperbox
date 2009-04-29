{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Score
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Named musical elements (e.g. notes, keys) within the NoteCtx monad
--
--------------------------------------------------------------------------------

module Mullein.Score (
    ScNote(..),
    Part, 
    Phrase,
    Motif,
    Bar,
    Unison,
    Bracket,
    Element,
    GraceNote,

    Note(..),
    NoteAttribute(..),

    part,

    phrase, repeated, fsrepeat,
    
    motif,
    primary, addOverlay,
--    notelist,

    rest, space, chord,
    
    
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

import qualified Mullein.AbcOutput as Abc
import Mullein.AbcNoteClass
import Mullein.Bracket
import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.LilyPondNoteClass
import qualified Mullein.LilyPondConvert as Ly
import qualified Mullein.LilyPondOutput as Ly
import Mullein.Pitch
import Mullein.SpellingMap ( naturalize )

import Data.Ratio
import Text.PrettyPrint.Leijen ( (<>) )






data NoteAttribute = Fingering Int
  deriving (Eq,Show)

data ScNote = ScNote Pitch [NoteAttribute]
  deriving (Show)

-- Synonyms for the common case where we approximate Haskore
type Part      = PartP ScNote
type Phrase    = PhraseP ScNote
type Motif     = MotifP ScNote
type Bar       = BarP ScNote
type Unison    = UnisonP ScNote
type Bracket   = BracketP ScNote
type Element   = ElementP ScNote
type GraceNote = GraceNoteP ScNote


class Note a b c | c -> a b where
  note :: a -> b -> c 

--------------------------------------------------------------------------------
-- instances


instance AbcNote ScNote where
  respell lset (ScNote p attrs)  = ScNote (naturalize lset p) attrs
  abcNote (ScNote p _) dm        = Abc.printNote p dm
  abcPitch (ScNote p _)          = Abc.printNote p 1
  inlineAnno (ScNote _ _attrs)   = Nothing -- this should handle fingerings at least...


instance LyNote ScNote where
  rewritePitch (ScNote new attrs) = do 
     old <- exchangePitch new
     return $ ScNote (Ly.relPitch old new) attrs

  rewritePitches _  = error "TODO"

  lyNote (ScNote p _) od = Ly.note p <> Ly.optDuration od
  lyPitch (ScNote p _)   = Ly.note p




--------------------------------------------------------------------------------


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


chord :: [e] -> Duration -> ElementP e
chord xs dur = Chord xs dur

{-
-- alternative to @note@ with more general type
(%%) :: e -> Duration -> ElementP e
(%%) pch dur = Note pch dur
-}

instance Note Pitch [NoteAttribute] ScNote where
  note p xs = ScNote p xs 

-- notes
cf :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
c  :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
cs :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
df :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
d  :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
ds :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
ef :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
e  :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
es :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
ff :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
f  :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
fs :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
gf :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
g  :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
gs :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
af :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
a  :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
as :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
bf :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
b  :: Note Pitch b c => Octave -> Duration -> b -> ElementP c
bs :: Note Pitch b c => Octave -> Duration -> b -> ElementP c


cf o dur attrs = Note (note (Pitch C Flat o) attrs) dur
c  o dur attrs = Note (note (Pitch C Nat o) attrs) dur
cs o dur attrs = Note (note (Pitch C Sharp o) attrs) dur
df o dur attrs = Note (note (Pitch D Flat o) attrs) dur
d  o dur attrs = Note (note (Pitch D Nat o) attrs) dur
ds o dur attrs = Note (note (Pitch D Sharp o) attrs) dur
ef o dur attrs = Note (note (Pitch E Flat o) attrs) dur
e  o dur attrs = Note (note (Pitch E Nat o) attrs) dur
es o dur attrs = Note (note (Pitch E Sharp o) attrs) dur
ff o dur attrs = Note (note (Pitch F Flat o) attrs) dur
f  o dur attrs = Note (note (Pitch F Nat o) attrs) dur
fs o dur attrs = Note (note (Pitch F Sharp o) attrs) dur
gf o dur attrs = Note (note (Pitch G Flat o) attrs) dur
g  o dur attrs = Note (note (Pitch G Nat o) attrs) dur
gs o dur attrs = Note (note (Pitch G Sharp o) attrs) dur
af o dur attrs = Note (note (Pitch A Flat o) attrs) dur
a  o dur attrs = Note (note (Pitch A Nat o) attrs) dur
as o dur attrs = Note (note (Pitch A Sharp o) attrs) dur
bf o dur attrs = Note (note (Pitch B Flat o) attrs) dur
b  o dur attrs = Note (note (Pitch B Nat o) attrs) dur
bs o dur attrs = Note (note (Pitch B Sharp o) attrs) dur



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



