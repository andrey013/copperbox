{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.AdvEvent
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Events with sequential concatenation - cf. an /advance/ vector
-- in text printing.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.AdvEvent
  ( 

    AdvEvent
  , runAdvEvent
  , symEvent
  , sym1

  ) where

import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Kernel.Base.RenderContext
import ZMidi.Basic.Kernel.Objects.Event

import ZMidi.Basic.Primitive.EventList hiding ( reverse, stretch )

import Control.Applicative
import Data.Monoid
import Prelude hiding ( reverse )


newtype DAdv = DAdv { getDAdv :: Double } 


instance Monoid DAdv where
  mempty = DAdv 0
  DAdv v1 `mappend` DAdv v2 = DAdv $ v1 + v2


newtype AdvEvent u a = AdvEvent
         { getAdvEvent :: RenderContext -> OnsetTime -> (a, DAdv, CatEvent) }

instance Functor (AdvEvent u) where
  fmap f ma = AdvEvent $ \ctx loc -> let (a,v1,w) = getAdvEvent ma ctx loc
                                     in (f a, v1, w)


--
-- NOTE - In Wumpus, (>>=) and (<*>) do not move the second arg 
-- by the advance vector of the first. We need to check this is 
-- the correct behaviour...
--

instance Applicative (AdvEvent u) where
  pure a    = AdvEvent $ \_   _   -> (a,mempty,mempty)
  mf <*> ma = AdvEvent $ \ctx loc -> 
                let (f,v1,w1) = getAdvEvent mf ctx loc
                    (a,v2,w2) = getAdvEvent ma ctx loc
                in (f a, v1 `mappend` v2, w1 `mappend` w2)



instance Monad (AdvEvent u) where
  return    = pure
  ma >>= k  = AdvEvent $ \ctx loc -> 
                let (a,v1,w1) = getAdvEvent ma ctx loc
                    (b,v2,w2) = getAdvEvent (k a) ctx loc
                in (b, v1 `mappend` v2, w1 `mappend` w2)


instance Monoid a => Monoid (AdvEvent u a) where
  mempty          = pure mempty
  ma `mappend` mb = AdvEvent $ \ctx loc -> 
                      let (a,v1,w1) = getAdvEvent ma ctx loc
                          (b,v2,w2) = getAdvEvent mb ctx (loc + getDAdv v1)
                      in (a `mappend` b, v1 `mappend` v2, w1 `mappend` w2)


 
runAdvEvent :: InterpretUnit u 
             => RenderContext -> u -> AdvEvent u a -> (a,u,CatEvent)
runAdvEvent ctx loc ma = 
    let bpm  = interp_bpm ctx
        dloc = normalize bpm loc
        (a,ddur,w1) = getAdvEvent ma ctx dloc
        udur        = dinterp bpm $ getDAdv ddur
    in (a,udur,w1)


symEvent :: InterpretUnit u => Event u a -> AdvEvent u a
symEvent ev = AdvEvent $ \ctx loc -> 
                let bpm         = interp_bpm ctx
                    uloc        = dinterp bpm loc
                    (a,udur,w1) = runEvent ctx uloc ev
                    ddur        = normalize bpm udur
                in (a, DAdv ddur, w1)


sym1 :: InterpretUnit u => AdvEvent u a -> Event u a
sym1 so = promoteLoc $ \ctx u -> runAdvEvent ctx u so

{-
simul :: (Monoid a, InterpretUnit u) => [Event u a] -> AdvEvent u a
simul es = AdvEvent $ \ctx dn -> 
                let bpm         = interp_bpm ctx
                    uloc        = dinterp bpm s
                    (a,udur,w1) = runEvent ctx uloc ev
                    ddur        = normalize bpm udur
                in (a, dn + ddur, w1)
-}


