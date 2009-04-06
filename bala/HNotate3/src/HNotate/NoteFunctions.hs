{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.NoteFunctions
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Alternative representation...
--
--------------------------------------------------------------------------------

module HNotate.NoteFunctions where

import HNotate.Duration
import HNotate.NoteList
import qualified HNotate.NamedElements as H

import Control.Applicative
-- import 

data Env = Env { unit_note_length :: Duration }
  deriving (Show)

type ElementF = Env -> Element

rest :: ElementF
rest = Rest <$> unit_note_length

c4    :: ElementF
d4    :: ElementF
e4    :: ElementF
f4    :: ElementF
g4    :: ElementF
a4    :: ElementF
b4    :: ElementF
cis4  :: ElementF
des4  :: ElementF
dis4  :: ElementF
ees4  :: ElementF
fis4  :: ElementF
ges4  :: ElementF
gis4  :: ElementF
aes4  :: ElementF
ais4  :: ElementF
bes4  :: ElementF
c4    = Note H.c4   <$> unit_note_length
d4    = Note H.d4   <$> unit_note_length
e4    = Note H.e4   <$> unit_note_length
f4    = Note H.f4   <$> unit_note_length
g4    = Note H.g4   <$> unit_note_length
a4    = Note H.a4   <$> unit_note_length
b4    = Note H.b4   <$> unit_note_length
cis4  = Note H.cis4 <$> unit_note_length
des4  = Note H.des4 <$> unit_note_length
dis4  = Note H.dis4 <$> unit_note_length
ees4  = Note H.ees4 <$> unit_note_length
fis4  = Note H.fis4 <$> unit_note_length
ges4  = Note H.ges4 <$> unit_note_length
gis4  = Note H.gis4 <$> unit_note_length
aes4  = Note H.aes4 <$> unit_note_length
ais4  = Note H.ais4 <$> unit_note_length
bes4  = Note H.bes4 <$> unit_note_length

c3    :: ElementF
d3    :: ElementF
e3    :: ElementF
f3    :: ElementF
g3    :: ElementF
a3    :: ElementF
b3    :: ElementF
cis3  :: ElementF
des3  :: ElementF
dis3  :: ElementF
ees3  :: ElementF
fis3  :: ElementF
ges3  :: ElementF
gis3  :: ElementF
aes3  :: ElementF
ais3  :: ElementF
bes3  :: ElementF
c3    = Note H.c3   <$> unit_note_length
d3    = Note H.d3   <$> unit_note_length
e3    = Note H.e3   <$> unit_note_length
f3    = Note H.f3   <$> unit_note_length
g3    = Note H.g3   <$> unit_note_length
a3    = Note H.a3   <$> unit_note_length
b3    = Note H.b3   <$> unit_note_length
cis3  = Note H.cis3 <$> unit_note_length
des3  = Note H.des3 <$> unit_note_length
dis3  = Note H.dis3 <$> unit_note_length
ees3  = Note H.ees3 <$> unit_note_length
fis3  = Note H.fis3 <$> unit_note_length
ges3  = Note H.ges3 <$> unit_note_length
gis3  = Note H.gis3 <$> unit_note_length
aes3  = Note H.aes3 <$> unit_note_length
ais3  = Note H.ais3 <$> unit_note_length
bes3  = Note H.bes3 <$> unit_note_length


c5    :: ElementF
d5    :: ElementF
e5    :: ElementF
f5    :: ElementF
g5    :: ElementF
a5    :: ElementF
b5    :: ElementF
cis5  :: ElementF
des5  :: ElementF
dis5  :: ElementF
ees5  :: ElementF
fis5  :: ElementF
ges5  :: ElementF
gis5  :: ElementF
aes5  :: ElementF
ais5  :: ElementF
bes5  :: ElementF
c5    = Note H.c5   <$> unit_note_length
d5    = Note H.d5   <$> unit_note_length
e5    = Note H.e5   <$> unit_note_length
f5    = Note H.f5   <$> unit_note_length
g5    = Note H.g5   <$> unit_note_length
a5    = Note H.a5   <$> unit_note_length
b5    = Note H.b5   <$> unit_note_length
cis5  = Note H.cis5 <$> unit_note_length
des5  = Note H.des5 <$> unit_note_length
dis5  = Note H.dis5 <$> unit_note_length
ees5  = Note H.ees5 <$> unit_note_length
fis5  = Note H.fis5 <$> unit_note_length
ges5  = Note H.ges5 <$> unit_note_length
gis5  = Note H.gis5 <$> unit_note_length
aes5  = Note H.aes5 <$> unit_note_length
ais5  = Note H.ais5 <$> unit_note_length
bes5  = Note H.bes5 <$> unit_note_length

-- Shorthands
du1   :: Duration
du1   = H.du1

du2   :: Duration
du2   = H.du2

du4   :: Duration
du4   = H.du4

du8   :: Duration
du8   = H.du8

du16  :: Duration
du16  = H.du16

du32  :: Duration
du32  = H.du32

du64  :: Duration
du64  = H.du64

du128 :: Duration
du128 = H.du128
