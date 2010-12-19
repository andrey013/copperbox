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



beat :: (MidiDuration -> Build ()) -> Build ()
beat mf = mf dquarter

shiftBeat :: (MidiDuration -> Build ()) -> Build ()
shiftBeat mf = rest (0.25 * dquarter) >> mf (0.75 * dquarter)


halfBeats :: (MidiDuration -> Build ()) -> (MidiDuration -> Build ()) -> Build () 
halfBeats mf mg = mf deighth >> mg deighth

pletBeats :: Int -> Int -> [MidiDuration -> Build ()] -> Build ()
pletBeats n d ps = mapM_ ($ unit_dur) ps
  where
    unit_dur = (dquarter * fromIntegral d) / (fromIntegral n)


drumPrim :: GMDrum -> MidiDuration -> Build ()
drumPrim gmd = \dur -> note dur (drumPitch gmd) 

flamPrim :: GMDrum -> GMDrum ->  MidiDuration -> Build ()
flamPrim d1 d2 =
    \dur -> note (0.25*dur) (drumPitch d1) >> note (0.75*dur) (drumPitch d2)



