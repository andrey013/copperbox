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

import Data.Monoid



-- | To generate output we need a Midi interpretation.
--

newtype MidiDjembe = MidiDjembe { getMidiDjembe :: MidiDuration -> Build () }


instance CStrokeBase MidiDjembe where
  rest_note     = MidiDjembe $ rest
  period        = MidiDjembe $ drumPrim Low_bongo       -- this should be low vol


instance CStrokeAnno MidiDjembe where
  optional      = id
  accent        = id 
  lead_in       = id 
  dominant      = id 
  other_hand    = id  


instance CStrokeBell MidiDjembe where
  add_bell      = id


--------------------------------------------------------------------------------  

barChannelTracks :: [Bar MidiDjembe] -> ZMidiRep
barChannelTracks = mconcat . map barChannelTrack



barChannelTrack :: Bar MidiDjembe -> ZMidiRep
barChannelTrack bar = singleTrack $ singleSection 9 120 $ [ barBeats bar ]

barBeats :: Bar MidiDjembe -> Build ()
barBeats = mapM_ groupBeats


groupBeats :: Group MidiDjembe -> Build ()
groupBeats = mapM_ makeBeat


-- Duration is always qn

makeBeat :: Beat MidiDjembe -> Build ()
makeBeat (I  a)      = beat $ getMidiDjembe a
makeBeat (S  a)      = shiftBeat $ getMidiDjembe a
makeBeat (Ha a b)    = halfBeats (getMidiDjembe a) (getMidiDjembe b)
makeBeat (Pl n d xs) = pletBeats n d (map getMidiDjembe xs)

