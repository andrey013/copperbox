{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.AdvObject
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - an AdvanceGraphic is a Event 
-- twinned with and advance vector.
-- 
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Objects.AdvObject
  (

  -- * Advance vector
    AdvanceVec

  -- * Advance-vector object and graphic
  , AdvObject
  , DAdvObject
  
  , runAdvObject

  , makeAdvObject
  , emptyAdvObject
  , blankAdvObject
  

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
-- AdvObject

type AdvDraw u = u -> CatPrim



-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- advance (width) vector as each character is drawn.
--
newtype AdvObject ctx u = AdvObject 
          { getAdvObject :: Event ctx u (AdvanceVec u, AdvDraw u) }

type instance DUnit (AdvObject ctx u) = u
type instance Ctx (AdvObject ctx) = ctx


type DAdvObject     = AdvObject Double






--------------------------------------------------------------------------------

instance (InterpretUnit u) => Monoid (AdvObject ctx u) where
  mempty  = blankAdvObject 0
  mappend = advplus


-- | Run an 'AdvObject' turning it into an 'LocIEvent'.
--
runAdvObject :: (CtxTempo ctx, InterpretUnit u)
             => AdvObject ctx u -> LocEvent ctx u (AdvanceVec u)
runAdvObject (AdvObject mf) = promoteLoc $ \ot -> 
   askCtx >>= \ctx -> 
   let (PrimW _ (v1,df)) = runEvent ctx mf
   in replaceAns v1 $ primEvent (df ot)


-- | Bild an 'AdvObject' from a query that generates the answer 
-- advance vector and a 'LocEvent' that generates the event.
--
makeAdvObject :: (CtxTempo ctx, InterpretUnit u )
              => Event ctx u (AdvanceVec u) -> ULocEvent ctx u 
              -> AdvObject ctx u
makeAdvObject evt gf = AdvObject body
  where
    body = askCtx >>= \ctx -> 
           let (PrimW _ v1) = runEvent ctx evt
               pf ot        = let (PrimW ca _) = runLocEvent ot ctx gf in ca
           in return (v1,pf)



-- | 'emptyAdvObjectAU' : @ AdvObject @
--
-- Build an empty 'AdvObject'.
-- 
-- The 'emptyAdvObject' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, the answer vector generated is
-- the zero vector @(V2 0 0)@.
-- 
emptyAdvObject :: InterpretUnit u => AdvObject ctx u
emptyAdvObject = blankAdvObject 0


blankAdvObject :: u -> AdvObject ctx u
blankAdvObject v1 = AdvObject $ pure (v1, const mempty)





--------------------------------------------------------------------------------
-- Combining AdvObjects


-- | Design note - this is rather /uncool/.
--
-- Here it would be nicer if PrimW didn\'t cover two cases - 
-- queries (Pure) and images (PrimW). However implementing this 
-- would double the amount of code and then require extra 
-- bind-like combinators to promote queries to images.
--
-- This is simulated in @appendW@ by dropping any graphic embedded 
-- in a PrimW (everything should be a query anyway). But it would 
-- be nicer in this particular case, if the type system enforced 
-- this.
--
appendW :: Num u 
        => (AdvanceVec u, AdvDraw u) 
        -> (AdvanceVec u, AdvDraw u) 
        -> (AdvanceVec u, AdvDraw u)
appendW (v0,tf0) (v1,tf1) = let tf = \ot -> tf0 ot `mappend` tf1 (ot + v0)
                            in (v0 + v1, tf)




-- | Primitive combination.
-- 
-- Move second object by the advance vector of the first. Sum 
-- both advance vectors.
--
advplus :: Num u 
        => AdvObject ctx u -> AdvObject ctx u -> AdvObject ctx u
advplus a b = AdvObject body
  where 
    body = askCtx >>= \ctx ->
           let (PrimW _ a1) = runEvent ctx (getAdvObject a)
               (PrimW _ a2) = runEvent ctx (getAdvObject b)
           in return (appendW a1 a2)




-- Helper for list concatenation.
-- 
listcat :: InterpretUnit u 
        => (AdvObject ctx u -> AdvObject ctx u -> AdvObject ctx u)
        -> [AdvObject ctx u] -> AdvObject ctx u
listcat _ []     = mempty
listcat op (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (b:bs) = go (acc `op` b) bs



-- AdvObject does not have the same ability to be concatenated
-- as PosObject - all the advance vector says is \"where to go 
-- next\". Nothing in the AdvObject tracks the boundary so we
-- cannot implement the Concat classes.

infixr 6 `advance`


-- | Draw the first AdvObject and use the advance vector to 
-- displace the second AdvObject.
--
-- The final answer is the sum of both advance vectors.
--
advance :: Num u 
        => AdvObject ctx u -> AdvObject ctx u -> AdvObject ctx u
advance = advplus
  

-- | Concatenate the list of AdvObjects with 'advance'.
--
advances :: InterpretUnit u 
         => [AdvObject ctx u] -> AdvObject ctx u
advances = listcat advance


-- | Combine the AdvObjects using the answer vector of the 
-- first object plus the separator to move the start of the second
-- object. 
--
advspace :: InterpretUnit u 
         => AdvanceVec u -> AdvObject ctx u -> AdvObject ctx u 
         -> AdvObject ctx u
advspace sep a b = a `advplus` blank `advplus` b
  where
    blank = blankAdvObject sep

-- | List version of 'nextSpace'.
--
evenspace :: InterpretUnit u 
          => AdvanceVec u -> [AdvObject ctx u] -> AdvObject ctx u
evenspace v = listcat (advspace v)



-- | Repeat the AdvObject @n@ times, moving each time with 
-- 'advance'.
--
advrepeat :: InterpretUnit u 
          => Int -> AdvObject ctx u -> AdvObject ctx u
advrepeat n = advances . replicate n


-- | Concatenate the list of AdvObjects, going next and adding
-- the separator at each step.
--
punctuate :: InterpretUnit u 
          => AdvObject ctx u -> [AdvObject ctx u] -> AdvObject ctx u
punctuate sep =  listcat (\a b -> a `advance` sep `advance` b)



-- | Render the supplied AdvObject, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: InterpretUnit u 
        => AdvanceVec u -> AdvObject ctx u -> AdvObject ctx u
advfill sv a = AdvObject body
  where 
    body = askCtx >>= \ctx ->
           let (PrimW _ (_,df)) = runEvent ctx (getAdvObject a)
           in return (sv,df)


