{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.MidiPrimitives
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

module Wumpus.Rhythm.Djembe.MidiPrimitives 
  where


import ZMidi.Emit                               -- package: zmidi-emit



beat :: (MidiDuration -> NoteList ()) -> NoteList ()
beat mf = mf dquarter

shiftBeat :: (MidiDuration -> NoteList ()) -> NoteList ()
shiftBeat mf = rest (0.25 * dquarter) >> mf (0.75 * dquarter)


halfBeats :: (MidiDuration -> NoteList ()) -> (MidiDuration -> NoteList ()) -> NoteList () 
halfBeats mf mg = mf deighth >> mg deighth

pletBeats :: Int -> Int -> [MidiDuration -> NoteList ()] -> NoteList ()
pletBeats n d ps = mapM_ ($ unit_dur) ps
  where
    unit_dur = (dquarter * fromIntegral d) / (fromIntegral n)


drumPrim :: GMDrum -> MidiDuration -> NoteList ()
drumPrim gmd = \dur -> note gmd dur

flamPrim :: GMDrum -> GMDrum ->  MidiDuration -> NoteList ()
flamPrim d1 d2 =
    \dur -> note d1 (0.25*dur) >> note d2 (0.75*dur)



