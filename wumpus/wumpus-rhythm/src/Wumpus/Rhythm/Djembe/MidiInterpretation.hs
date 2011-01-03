{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.MidiInterpretation
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Djembe
--
--------------------------------------------------------------------------------

module Wumpus.Rhythm.Djembe.MidiInterpretation where

import Wumpus.Rhythm.Djembe.Base
import Wumpus.Rhythm.Djembe.MidiPrimitives

import ZMidi.Emit                               -- package: zmidi-emit

import Data.List
import Data.Monoid



-- | To generate output we need a Midi interpretation.
--

newtype MidiDjembe = MidiDjembe { getMidiDjembe :: MidiDuration -> NoteList () }


instance CStrokeBase MidiDjembe where
  rest_note     = MidiDjembe $ rest
  period        = MidiDjembe $ drumPrim low_bongo       -- this should be low vol


instance CStrokeAnno MidiDjembe where
  optional      = id
  accent        = id 
  lead_in       = id 
  dominant      = id 
  other_hand    = id  


instance CStrokeBell MidiDjembe where
  add_bell      = id


--------------------------------------------------------------------------------  

barChannelTracks :: [Bar MidiDjembe] -> HiMidi
barChannelTracks = foldl' fn hiMidi 
  where
    fn ac e = ac `addT` makePercTrack e



barChannelTrack :: Bar MidiDjembe -> HiMidi
barChannelTrack bar = hiMidi `addT` makePercTrack bar

makePercTrack :: Bar MidiDjembe -> Track
makePercTrack bar = track 9 chan_strm
  where
    chan_strm = section 120 $ barBeats bar



barBeats :: Bar MidiDjembe -> NoteList ()
barBeats = mapM_ groupBeats


groupBeats :: Group MidiDjembe -> NoteList ()
groupBeats = mapM_ makeBeat


-- Duration is always qn

makeBeat :: Beat MidiDjembe -> NoteList ()
makeBeat (I  a)      = beat $ getMidiDjembe a
makeBeat (S  a)      = shiftBeat $ getMidiDjembe a
makeBeat (Ha a b)    = halfBeats (getMidiDjembe a) (getMidiDjembe b)
makeBeat (Pl n d xs) = pletBeats n d (map getMidiDjembe xs)

