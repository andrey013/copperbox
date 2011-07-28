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
  
  , runAdvEvent

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

-- | Advance vectors provide an idiom for consecutive events,
-- unlike LocEvent (which has no notion of end time as it is a 
-- functional type).
--
-- Type alias for u (duration).
--
type AdvanceVec u = u



--------------------------------------------------------------------------------
-- AdvEvent

type AdvDraw itbl u = u -> CatPrim itbl



-- | /Advance vector/ event - this models moving the 
-- /onset time/ for the next event by the advance vector of the 
-- current event.
--
newtype AdvEvent itbl ctx u = AdvEvent 
          { getAdvEvent :: Query itbl ctx u (AdvanceVec u, AdvDraw itbl u) }

type instance DUnit (AdvEvent itbl ctx u) = u


type DAdvEvent itbl ctx    = AdvEvent itbl ctx Double 






--------------------------------------------------------------------------------

instance (InterpretUnit u) => Monoid (AdvEvent itbl ctx u) where
  mempty  = blankAdvEvent 0
  mappend = advplus


-- | Run an 'AdvEvent' turning it into an 'LocEvent'.
--
runAdvEvent :: InterpretUnit u
            => AdvEvent itbl ctx u -> LocEvent itbl ctx u (AdvanceVec u)
runAdvEvent (AdvEvent mf) = promoteLoc $ \ot -> 
   askCtx >>= \ctx -> 
   let (v1,df) = runQuery ctx mf
   in replaceAns v1 $ primEvent (df ot)


-- | Build an 'AdvEvent' from a query that generates the answer 
-- advance vector and a 'LocEvent' that generates the event.
--
makeAdvEvent :: InterpretUnit u
             => Query itbl ctx u (AdvanceVec u) -> ULocEvent itbl ctx u 
             -> AdvEvent itbl ctx u
makeAdvEvent mq gf = AdvEvent body
  where
    body = askCtx >>= \ctx -> 
           let v1     = runQuery ctx mq
               pf ot  = let (PrimW ca _) = runLocEvent ot ctx gf in ca
           in return (v1,pf)



-- | 'emptyAdvEventAU' : @ AdvEvent @
--
-- Build an empty 'AdvEvent'.
-- 
-- The 'emptyAdvEvent' is treated as a /null primitive/ by 
-- @Majalan-Core@ and has effect, the answer generated is
-- the zero vector @0@.
-- 
emptyAdvEvent :: InterpretUnit u => AdvEvent itbl ctx u
emptyAdvEvent = blankAdvEvent 0


blankAdvEvent :: u -> AdvEvent itbl ctx u
blankAdvEvent v1 = AdvEvent $ pure (v1, const mempty)





--------------------------------------------------------------------------------
-- Combining AdvEvents


-- | Sequential append - one event after the other.
-- 
appendW :: Num u 
        => (AdvanceVec u, AdvDraw itbl u) 
        -> (AdvanceVec u, AdvDraw itbl u) 
        -> (AdvanceVec u, AdvDraw itbl u)
appendW (v0,tf0) (v1,tf1) = let tf = \ot -> tf0 ot `mappend` tf1 (ot + v0)
                            in (v0 + v1, tf)




-- | Sequential combination.
-- 
-- Move second object by the advance vector of the first. Sum 
-- both advance vectors.
--
advplus :: Num u 
        => AdvEvent itbl ctx u -> AdvEvent itbl ctx u -> AdvEvent itbl ctx u
advplus a b = AdvEvent body
  where 
    body = askCtx >>= \ctx ->
           let a1 = runQuery ctx (getAdvEvent a)
               a2 = runQuery ctx (getAdvEvent b)
           in return (appendW a1 a2)






-- Helper for list concatenation.
-- 
listcat :: InterpretUnit u 
        => (AdvEvent itbl ctx u -> AdvEvent itbl ctx u -> AdvEvent itbl ctx u)
        -> [AdvEvent itbl ctx u] -> AdvEvent itbl ctx u
listcat _ []     = mempty
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
advance :: Num u 
        => AdvEvent itbl ctx u -> AdvEvent itbl ctx u -> AdvEvent itbl ctx u
advance = advplus
  

-- | Concatenate the list of AdvEvents with 'advance'.
--
advances :: InterpretUnit u 
         => [AdvEvent itbl ctx u] -> AdvEvent itbl ctx u
advances = listcat advance


-- | Combine the AdvEvents using the answer vector of the 
-- first object plus the separator to move the start of the second
-- object. 
--
advspace :: InterpretUnit u 
         => AdvanceVec u -> AdvEvent itbl ctx u -> AdvEvent itbl ctx u 
         -> AdvEvent itbl ctx u
advspace sep a b = a `advplus` blank `advplus` b
  where
    blank = blankAdvEvent sep

-- | List version of 'nextSpace'.
--
evenspace :: InterpretUnit u 
          => AdvanceVec u -> [AdvEvent itbl ctx u] -> AdvEvent itbl ctx u
evenspace v = listcat (advspace v)



-- | Repeat the AdvEvent @n@ times, moving each time with 
-- 'advance'.
--
advrepeat :: InterpretUnit u 
          => Int -> AdvEvent itbl ctx u -> AdvEvent itbl ctx u
advrepeat n = advances . replicate n


-- | Concatenate the list of AdvEvents, going next and adding
-- the separator at each step.
--
punctuate :: InterpretUnit u 
          => AdvEvent itbl ctx u -> [AdvEvent itbl ctx u] -> AdvEvent itbl ctx u
punctuate sep =  listcat (\a b -> a `advance` sep `advance` b)



-- | Render the supplied AdvEvent, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: InterpretUnit u 
        => AdvanceVec u -> AdvEvent itbl ctx u -> AdvEvent itbl ctx u
advfill sv a = AdvEvent body
  where 
    body = askCtx >>= \ctx ->
           let (_,df) = runQuery ctx (getAdvEvent a)
           in return (sv,df)


