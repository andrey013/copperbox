{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.AdvEvent
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

module ZSnd.Basic.Kernel.Objects.AdvEvent
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

import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Base.WrappedPrimitive
import ZSnd.Basic.Kernel.Objects.Basis
import ZSnd.Basic.Kernel.Objects.LocEvent


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

type AdvDraw u = u -> CatPrim



-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- advance (width) vector as each character is drawn.
--
newtype AdvEvent ctx u = AdvEvent 
          { getAdvEvent :: Query ctx u (AdvanceVec u, AdvDraw u) }

type instance DUnit (AdvEvent ctx u) = u
type instance Ctx (AdvEvent ctx) = ctx


type DAdvEvent     = AdvEvent Double






--------------------------------------------------------------------------------

instance (InterpretUnit u) => Monoid (AdvEvent ctx u) where
  mempty  = blankAdvEvent 0
  mappend = advplus


-- | Run an 'AdvEvent' turning it into an 'LocIEvent'.
--
runAdvEvent :: (CtxTempo ctx, InterpretUnit u)
             => AdvEvent ctx u -> LocEvent ctx u (AdvanceVec u)
runAdvEvent (AdvEvent mf) = promoteLoc $ \ot -> 
   askCtx >>= \ctx -> 
   let (v1,df) = runQuery ctx mf
   in replaceAns v1 $ primEvent (df ot)


-- | Build an 'AdvEvent' from a query that generates the answer 
-- advance vector and a 'LocEvent' that generates the event.
--
makeAdvEvent :: (CtxTempo ctx, InterpretUnit u )
              => Query ctx u (AdvanceVec u) -> ULocEvent ctx u 
              -> AdvEvent ctx u
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
-- @Wumpus-Core@ and is not drawn, the answer vector generated is
-- the zero vector @(V2 0 0)@.
-- 
emptyAdvEvent :: InterpretUnit u => AdvEvent ctx u
emptyAdvEvent = blankAdvEvent 0


blankAdvEvent :: u -> AdvEvent ctx u
blankAdvEvent v1 = AdvEvent $ pure (v1, const mempty)





--------------------------------------------------------------------------------
-- Combining AdvEvents


-- | Sequential append - one event after the other.
-- 
appendW :: Num u 
        => (AdvanceVec u, AdvDraw u) 
        -> (AdvanceVec u, AdvDraw u) 
        -> (AdvanceVec u, AdvDraw u)
appendW (v0,tf0) (v1,tf1) = let tf = \ot -> tf0 ot `mappend` tf1 (ot + v0)
                            in (v0 + v1, tf)




-- | Sequential combination.
-- 
-- Move second object by the advance vector of the first. Sum 
-- both advance vectors.
--
advplus :: Num u 
        => AdvEvent ctx u -> AdvEvent ctx u -> AdvEvent ctx u
advplus a b = AdvEvent body
  where 
    body = askCtx >>= \ctx ->
           let a1 = runQuery ctx (getAdvEvent a)
               a2 = runQuery ctx (getAdvEvent b)
           in return (appendW a1 a2)






-- Helper for list concatenation.
-- 
listcat :: InterpretUnit u 
        => (AdvEvent ctx u -> AdvEvent ctx u -> AdvEvent ctx u)
        -> [AdvEvent ctx u] -> AdvEvent ctx u
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
        => AdvEvent ctx u -> AdvEvent ctx u -> AdvEvent ctx u
advance = advplus
  

-- | Concatenate the list of AdvEvents with 'advance'.
--
advances :: InterpretUnit u 
         => [AdvEvent ctx u] -> AdvEvent ctx u
advances = listcat advance


-- | Combine the AdvEvents using the answer vector of the 
-- first object plus the separator to move the start of the second
-- object. 
--
advspace :: InterpretUnit u 
         => AdvanceVec u -> AdvEvent ctx u -> AdvEvent ctx u 
         -> AdvEvent ctx u
advspace sep a b = a `advplus` blank `advplus` b
  where
    blank = blankAdvEvent sep

-- | List version of 'nextSpace'.
--
evenspace :: InterpretUnit u 
          => AdvanceVec u -> [AdvEvent ctx u] -> AdvEvent ctx u
evenspace v = listcat (advspace v)



-- | Repeat the AdvEvent @n@ times, moving each time with 
-- 'advance'.
--
advrepeat :: InterpretUnit u 
          => Int -> AdvEvent ctx u -> AdvEvent ctx u
advrepeat n = advances . replicate n


-- | Concatenate the list of AdvEvents, going next and adding
-- the separator at each step.
--
punctuate :: InterpretUnit u 
          => AdvEvent ctx u -> [AdvEvent ctx u] -> AdvEvent ctx u
punctuate sep =  listcat (\a b -> a `advance` sep `advance` b)



-- | Render the supplied AdvEvent, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: InterpretUnit u 
        => AdvanceVec u -> AdvEvent ctx u -> AdvEvent ctx u
advfill sv a = AdvEvent body
  where 
    body = askCtx >>= \ctx ->
           let (_,df) = runQuery ctx (getAdvEvent a)
           in return (sv,df)


