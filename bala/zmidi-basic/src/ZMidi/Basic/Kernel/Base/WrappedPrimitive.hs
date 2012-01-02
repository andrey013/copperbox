{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Base.WrappedPrimitive
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Wrapped Primitives supporting concatenation.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Base.WrappedPrimitive
  ( 

    primI
  , primOO


  ) where

import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Primitive.Syntax

import ZMidi.Core                               -- package: zmidi-core


--
-- Actually this is a bit crummy, we are only supporting 1 event 
-- long primitives when primitive can support many events.
-- 
-- Thus we are losing the cheap transformation that Primitive is 
-- designed for.
--


primI :: OnsetTime -> MidiVoiceEvent -> EventList
primI ot e = eventList1 (ot, instant $ VoiceEvent e)

primOO :: OnsetTime -> MidiVoiceEvent -> Double -> MidiVoiceEvent -> EventList
primOO ot e0 drn e1 = 
    eventList1 (ot, onoff (VoiceEvent e0) drn (VoiceEvent e1))

