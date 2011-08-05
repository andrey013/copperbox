{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Objects.AdvEvent
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Event - an AdvanceEvent is a Event twinned with an 
-- advance vector. 
--
-- This models the concatenative / sequential notes whereas 
-- @LocEvent@ models time lines.
-- 
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Objects.AdvEvent
  (

  -- * Advance vector
    AdvanceVec

  -- * Advance-vector object and graphic
  , AdvEvent
  , DAdvEvent
  , UAdvEvent

  , runAdvEvent

  , promoteAdv
  , makeAdvEvent

  , emptyAdvEvent
  , blankAdvEvent
  

  -- * Composition
  , advance
  , advances
  , advspace
  , evenspace

  , advrepeat
  , punctuate
  , advfill

  ) where

import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.LocEvent

import Majalan.Core                             -- package: majalan-core


import Control.Applicative
import Data.Monoid




--------------------------------------------------------------------------------

-- Advance vectors provide an mechinism for consecutive events,
-- unlike LocEvent.
--
-- If an Event produces its width, then a consecutive event can 
-- start at the (comment) onset plus the width of the previous 
-- event.
--


-- Type alias for u (duration).
--
type AdvanceVec u = u 

type DAdvVec = AdvanceVec Double



--------------------------------------------------------------------------------
-- AdvEvent


-- | /Advance vector/ event - this models moving the 
-- /onset time/ for the next event by the advance vector of the 
-- current event.
--
newtype AdvEvent ctx u a = AdvEvent 
          { getAdvEvent :: Context ctx -> OnsetDbl -> DAdvVec 
                        -> (a, DAdvVec, CatPrim) }

type instance DUnit (AdvEvent ctx u a) = u
type instance UCtx  (AdvEvent ctx u)   = ctx


type DAdvEvent ctx a   = AdvEvent ctx Double a

type UAdvEvent ctx u   = AdvEvent ctx u (UNil u)


instance Num u => Functor (AdvEvent ctx u) where
  fmap f mf = AdvEvent $ \r o s -> 
              let (a,s1,w1) = getAdvEvent mf r o s in (f a,s1,w1)

--
-- AdvanceVec is constructed with the State monad.
--
-- The only place we need to be mindful of it as when we create 
-- fresh AdvEvents.
--


instance InterpretUnit u => Applicative (AdvEvent ctx u) where
  pure a    = AdvEvent $ \_ _ s -> (a,s,mempty)
  mf <*> ma = AdvEvent $ \r o s -> 
              let (f,s1,w1) = getAdvEvent mf r o s 
                  (a,s2,w2) = getAdvEvent ma r o s1
              in (f a, s2, w1 `mappend` w2)



instance InterpretUnit u => Monad (AdvEvent ctx u) where
  return a  = AdvEvent $ \_ _ s -> (a,s,mempty)
  mf >>= k  = AdvEvent $ \r o s -> 
              let (a,s1,w1) = getAdvEvent mf r o s
                  (b,s2,w2) = getAdvEvent (k a) r o s1
              in (b, s2, w1 `mappend` w2)


instance InterpretUnit u => ContextM (AdvEvent ctx u) where
  askCtx          = AdvEvent $ \r _ s -> (r, s, mempty)
  asksCtx fn      = AdvEvent $ \r _ s -> (fn r, s, mempty)
  localize upd ma = AdvEvent $ \r o s -> getAdvEvent ma (upd r) o s


instance (Monoid a, InterpretUnit u) => Monoid (AdvEvent ctx u a) where
  mempty = AdvEvent $ \_ _ s -> (mempty, s, mempty)
  ma `mappend` mb = AdvEvent $ \r o s -> 
                    let (a,s1,w1) = getAdvEvent ma r o s
                        (b,s2,w2) = getAdvEvent mb r o s1
                    in (a `mappend` b, s2, w1 `mappend` w2)



-- Running an AdvEvent produces a LocEvent

runAdvEvent :: InterpretUnit u => AdvEvent ctx u a -> LocEvent ctx u a
runAdvEvent mf = promoteLoc $ \ot -> 
    askCtx >>= \ctx -> 
    let dot      = normalize (ctx_tempo ctx) ot
        (a,_,ca) = getAdvEvent mf ctx dot 0
    in replaceAns a $ primEvent ca
  

instance UConvert AdvEvent where
  uconvF = uconvAdvEventF
  uconvZ = uconvAdvEventZ


uconvAdvEventF :: (InterpretUnit u, InterpretUnit u1, Functor t) 
               => AdvEvent ctx u (t u) -> AdvEvent ctx u1 (t u1)
uconvAdvEventF ma = AdvEvent $ \ r o s -> 
                    let tempo = ctx_tempo r
                        (a,s1,ca) = getAdvEvent ma r o s
                    in (uconvertF tempo a, s1, ca)

uconvAdvEventZ :: (InterpretUnit u, InterpretUnit u1) 
               => AdvEvent ctx u a -> AdvEvent ctx u1 a
uconvAdvEventZ ma = AdvEvent $ \ r o s -> getAdvEvent ma r o s



--------------------------------------------------------------------------------

promoteAdv :: InterpretUnit u
           => (u -> Event ctx u (a,AdvanceVec u)) -> AdvEvent ctx u a
promoteAdv k = AdvEvent $ \r o s -> 
               let uo           = dinterp (ctx_tempo r) o
                   us           = dinterp (ctx_tempo r) s
                   ((a,v1),w1)  = runEvent r (k $ uo + us)
                   dv1          = normalize (ctx_tempo r) v1
               in (a, s + dv1, w1)

--
-- Note - dv1 is added to state (AdvanceVec).
--
-- The onset time is always static - it is the onset of the very
-- first AdvEvent (possibly chained by mappend or monadic bind).
-- When we render the promoted event, we need to add the current 
-- state (AdvanceVec) to the staic onset time, so the promoted 
-- event will be rendered at the correct position.
--

-- | Build an 'AdvEvent' from a query that generates the answer 
-- advance vector and a 'LocEvent' that generates the event.
--
makeAdvEvent :: InterpretUnit u
             => Query ctx u (AdvanceVec u) -> LocEvent ctx u a
             -> AdvEvent ctx u a
makeAdvEvent mq gf = AdvEvent $ \r o s -> 
      let v1     = runQuery r mq
          dv1    = normalize (ctx_tempo r) v1
          uo     = dinterp (ctx_tempo r) o
          us     = dinterp (ctx_tempo r) s
          (a,w1) = runLocEvent (uo + us) r gf
      in (a, s + dv1, w1)



-- | 'emptyAdvEventAU' : @ AdvEvent @
--
-- Build an empty 'AdvEvent'.
-- 
-- The 'emptyAdvEvent' is treated as a /null primitive/ by 
-- @Majalan-Core@ and has effect, the answer generated is
-- the zero vector @0@.
-- 
emptyAdvEvent :: (Monoid a, InterpretUnit u) => AdvEvent ctx u a
emptyAdvEvent = pure mempty


blankAdvEvent :: (Monoid a, InterpretUnit u) => u -> AdvEvent ctx u a
blankAdvEvent v1 = AdvEvent $ \r _ s -> 
      let dv1 = normalize (ctx_tempo r) v1 in (mempty, s + dv1, mempty)




-- Helper for list concatenation.
-- 
listcat :: (InterpretUnit u, Monoid a) 
        => (AdvEvent ctx u a -> AdvEvent ctx u a -> AdvEvent ctx u a)
        -> [AdvEvent ctx u a] -> AdvEvent ctx u a
listcat _ []      = mempty
listcat op (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (b:bs) = go (acc `op` b) bs



-- AdvEvent does not have the same ability to be concatenated
-- as PosEvent - all the advance vector says is \"where to go 
-- next\". Nothing in the AdvEvent tracks the boundary so we
-- cannot implement the Concat classes.

infixr 6 `advance`


-- | Draw the first AdvEvent and use the advance vector to 
-- displace the second AdvEvent.
--
-- The final answer is the sum of both advance vectors.
--
advance :: (InterpretUnit u, Monoid a)
        => AdvEvent ctx u a -> AdvEvent ctx u a -> AdvEvent ctx u a
advance = mappend
  

-- | Concatenate the list of AdvEvents with 'advance'.
--
advances :: (InterpretUnit u, Monoid a) 
         => [AdvEvent ctx u a] -> AdvEvent ctx u a
advances = mconcat


-- | Combine the AdvEvents using the answer vector of the 
-- first object plus the separator to move the start of the second
-- object. 
--
advspace :: (InterpretUnit u, Monoid a)
         => AdvanceVec u -> AdvEvent ctx u a -> AdvEvent ctx u a
         -> AdvEvent ctx u a
advspace sep a b = a `mappend` blank `mappend` b
  where
    blank = blankAdvEvent sep

-- | List version of 'nextSpace'.
--
evenspace :: (InterpretUnit u, Monoid a) 
          => AdvanceVec u -> [AdvEvent ctx u a] -> AdvEvent ctx u a
evenspace v = listcat (advspace v)



-- | Repeat the AdvEvent @n@ times, moving each time with 
-- 'advance'.
--
advrepeat :: (InterpretUnit u, Monoid a) 
          => Int -> AdvEvent ctx u a -> AdvEvent ctx u a
advrepeat n = advances . replicate n


-- | Concatenate the list of AdvEvents, going next and adding
-- the separator at each step.
--
punctuate :: (InterpretUnit u, Monoid a) 
          => AdvEvent ctx u a -> [AdvEvent ctx u a] -> AdvEvent ctx u a
punctuate sep =  listcat (\a b -> a `mappend` sep `mappend` b)



-- | Render the supplied AdvEvent, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: InterpretUnit u 
        => AdvanceVec u -> AdvEvent ctx u a -> AdvEvent ctx u a
advfill sv mf = AdvEvent $ \r o s -> 
    let (a,_,ca) = getAdvEvent mf r o s
        dv1      = normalize (ctx_tempo r) sv 
    in (a,dv1,ca)


