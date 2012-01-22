{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.TraceOutput
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output via a trace (i.e a writer monad).
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.TraceOutput
  ( 

    CtxScore
  , trace
  , reverse
  , stretch


  , Phrase
  , phrase
  , event1

  ) where

import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Kernel.Base.RenderContext
import ZMidi.Basic.Kernel.Objects.Event

import ZMidi.Basic.Primitive.EventList hiding ( reverse, stretch )
import qualified ZMidi.Basic.Primitive.EventList as Prim
import ZMidi.Basic.Primitive.RenderMidi

import ZMidi.Core                               -- package: zmidi-core

import Control.Applicative
import Data.Monoid
import Prelude hiding ( reverse )



newtype CtxScore a = CtxScore
         { getCtxScore :: RenderContext -> (a, EventList) }

instance Functor CtxScore where
  fmap f ma = CtxScore $ \ctx -> let (a,w) = getCtxScore ma ctx
                                 in (f a, w)

instance Applicative CtxScore where
  pure a    = CtxScore $ \_ -> (a,mempty)
  mf <*> ma = CtxScore $ \ctx -> 
                let (f,w1) = getCtxScore mf ctx
                    (a,w2) = getCtxScore ma ctx
                in (f a, w1 `mappend` w2)

instance Monad CtxScore where
  return    = pure
  ma >>= k  = CtxScore $ \ctx -> 
                let (a,w1) = getCtxScore ma ctx
                    (b,w2) = getCtxScore (k a) ctx
                in (b, w1 `mappend` w2)


instance Monoid a => Monoid (CtxScore a) where
  mempty          = pure mempty
  ma `mappend` mb = CtxScore $ \ctx -> 
                      let (a,w1) = getCtxScore ma ctx
                          (b,w2) = getCtxScore mb ctx
                      in (a `mappend` b, w1 `mappend` w2)





modifyW :: (EventList -> EventList) -> CtxScore a -> CtxScore a
modifyW f ma = CtxScore $ \ctx -> let (a,w) = getCtxScore ma ctx
                                  in (a, f w)

reverse :: CtxScore a -> CtxScore a
reverse = modifyW Prim.reverse

stretch :: Double -> CtxScore a -> CtxScore a
stretch sx = modifyW (Prim.stretch sx)

trace :: RenderContext -> CtxScore a -> MidiFile
trace ctx ma = genFormat1 $ snd $ getCtxScore ma ctx
 
                          
--------------------------------------------------------------------------------
-- Phrase



newtype Phrase a = Phrase
         { getPhrase :: RenderContext -> (a, CatEvent) }

instance Functor Phrase where
  fmap f ma = Phrase $ \ctx -> let (a,w) = getPhrase ma ctx in (f a, w)

instance Applicative Phrase where
  pure a    = Phrase $ \_ -> (a,mempty)
  mf <*> ma = Phrase $ \ctx -> 
                let (f,w1) = getPhrase mf ctx
                    (a,w2) = getPhrase ma ctx
                in (f a, w1 `mappend` w2)

instance Monad Phrase where
  return    = pure
  ma >>= k  = Phrase $ \ctx -> 
                let (a,w1) = getPhrase ma ctx
                    (b,w2) = getPhrase (k a) ctx
                in (b, w1 `mappend` w2)


instance Monoid a => Monoid (Phrase a) where
  mempty          = pure mempty
  ma `mappend` mb = Phrase $ \ctx -> 
                      let (a,w1) = getPhrase ma ctx
                          (b,w2) = getPhrase mb ctx
                      in (a `mappend` b, w1 `mappend` w2)



phrase :: Phrase a -> CtxScore a
phrase ma = CtxScore $ \ctx -> let (a,w) = getPhrase ma ctx
                               in (a, eventList w) 


event1 :: InterpretUnit u => u -> Event u a -> Phrase a
event1 ot ma = Phrase $ \ctx -> let (a,_,w) = runEvent ctx ot ma in (a, w)