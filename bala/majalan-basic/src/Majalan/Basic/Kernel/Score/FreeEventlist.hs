{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Score.FreeEventlist
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Generate a event list. Events can be freely placed on the 
-- timeline, they do not have to be sequential (the timeline is 
-- sorted before rendering).
-- 
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Score.FreeEventlist
  (

    renderEventlist
  , renderEventlistU  


  -- * Event list
  , Eventlist
  , DEventlist

  , runEventlist
  , execEventlist
  , evalEventlist

  , event
  , eventi
  , eventl
  , eventli


  ) where


import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.LocEvent

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Control.Monad
import Data.Monoid


--------------------------------------------------------------------------------



renderEventlist :: Context ctx -> Eventlist ctx u a -> Maybe RScore
renderEventlist ctx mf = catPrimToScoreMb $ execEventlist ctx mf


renderEventlistU :: Context ctx -> Eventlist ctx u a -> RScore
renderEventlistU ctx mf = maybe fk id $ renderEventlist ctx mf
  where
    fk = error "renderEventlistU - empty score." 





newtype Eventlist ctx u a   = Eventlist { 
          getEventlist :: Context ctx -> (a, CatPrim) }


type instance DUnit (Eventlist ctx u a) = u
type instance UCtx  (Eventlist ctx u)   = ctx


type DEventlist ctx a    = Eventlist ctx Double a


-- Functor

instance Functor (Eventlist ctx u) where
  fmap f ma = Eventlist $ \ctx -> 
                let (a,w) = getEventlist ma ctx in (f a,w)



-- Applicative

instance Applicative (Eventlist ctx u) where
  pure a    = Eventlist $ \_   -> (a, mempty)
  mf <*> ma = Eventlist $ \ctx -> 
                let (f,w1) = getEventlist mf ctx
                    (a,w2) = getEventlist ma ctx
                in (f a, w1 `mappend` w2)


-- Monad

instance Monad (Eventlist ctx u) where
  return a  = Eventlist $ \_   -> (a, mempty)
  ma >>= k  = Eventlist $ \ctx -> 
                let (a,w1) = getEventlist ma ctx
                    (b,w2) = (getEventlist . k) a ctx
                in (b,w1 `mappend` w2)
                               


-- ContextM

instance ContextM (Eventlist ctx u) where
  askCtx          = Eventlist $ \ctx -> (ctx, mempty)
  asksCtx f       = Eventlist $ \ctx -> (f ctx, mempty)
  localize upd ma = Eventlist $ \ctx -> getEventlist ma (upd ctx)





runEventlist :: Context ctx -> Eventlist ctx u a -> (a, CatPrim)
runEventlist ctx ma = getEventlist ma ctx

-- | Run the notelist returning only the output it produces, drop
-- any answer from the monadic computation.
--
execEventlist :: Context ctx -> Eventlist ctx u a -> CatPrim
execEventlist ctx ma = snd $ runEventlist ctx ma

-- | Run the notelist ignoring the output it produces, return the 
-- answer from the monadic computation.
--
-- Note - this useful for testing, generally one would want the 
-- opposite behaviour (return the notelist, ignore than the 
-- answer).
-- 
evalEventlist :: Context ctx -> Eventlist ctx u a -> a
evalEventlist ctx ma = fst $ runEventlist ctx ma


trace :: CatPrim -> Eventlist ctx u ()
trace a = Eventlist $ \_ -> ((), a)

--------------------------------------------------------------------------------





-- | Add an event to the note list taking settings from the 
-- /context/. 
--
-- This function is the /forgetful/ version of 'notei'. 
-- Commonly, it is used to add 'UEvent' objects which 
-- have no /answer/.
-- 
event :: Event ctx u a -> Eventlist ctx u ()
event gf = askCtx >>= \ctx -> 
           let (_,w1) = runEvent ctx gf
           in trace w1 >> return ()




-- | Add an event to the note list taking settings from the 
-- /context/. 
-- 
-- The event is logged in the Trace monad, and the result is 
-- returned.
-- 
eventi :: Event ctx u a -> Eventlist ctx u a
eventi gf = askCtx >>= \ctx -> 
            let (a,w1) = runEvent ctx gf 
            in trace w1 >> return a
            


-- | Add an event to the note list at the supplied onset, taking
-- settings from the /context/. 
--
-- This function is the /forgetful/ version of 'notei'. 
-- Commonly, it is used to add 'ULocEvent' objects which 
-- have no /answer/.
-- 
eventl :: InterpretUnit u 
       => u -> LocEvent ctx u a -> Eventlist ctx u ()
eventl ot evt = eventli ot evt >> return ()



-- | Add an event to the note list at the supplied onset, taking
-- settings from the /context/. 
--
-- The event is logged in the Trace monad, and the result is 
-- returned.
-- 
eventli :: InterpretUnit u 
        => u -> LocEvent ctx u a -> Eventlist ctx u a
eventli ot gf = askCtx >>= \ctx -> 
                let (a,w1) = runLocEvent ot ctx gf
                in trace w1 >> return a


--
-- Design note
--
-- There doesn\'t seem to be much merit to providing an analogue 
-- to grid in Wumpus-Basic. An analogue could likely be achieved 
-- just by using a different unit for onset time.
--  

