{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.AdvanceGraphic
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - an AdvanceGraphic is a Graphic 
-- twinned with and advance vector.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.AdvanceGraphic
  (

  -- * Advance-vector object and graphic
    AdvanceObject
  , DAdvanceObject
  
  , AdvGraphic
  , DAdvGraphic

  , makeAdvanceObject
  , emptyAdvanceObject

  , runAdvanceObject

  -- * Composition
  , next
  , nexts
  , nextSpace
  , evenspace

  , repeatnext
  , punctuate
  , advfill

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Monoid



type AdvDraw u = Point2 u -> GraphicAns u


-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- advance (width) vector as each character is drawn.
--
newtype AdvanceObject u = AdvanceObject 
          { getAdvanceObject :: CF (Vec2 u, AdvDraw u) }

type instance DUnit (AdvanceObject u) = u

type DAdvanceObject     = AdvanceObject Double


type AdvGraphic u       = LocImage u (Vec2 u)
type DAdvGraphic        = AdvGraphic Double


--------------------------------------------------------------------------------

instance (InterpretUnit u) => Monoid (AdvanceObject u) where
  mempty  = emptyAdvanceObject
  mappend = advplus


-- | 'makeAdvanceObject' : @ loc_context_function * graphic -> AdvanceObject @
--
-- Build an 'AdvanceObject' from a context function ('CF') that 
-- generates the answer displacement vector and a 'LocGraphic' 
-- that draws the 'AdvanceObject'.
--
makeAdvanceObject :: Query (Vec2 u)
                  -> LocGraphic u 
                  -> AdvanceObject u
makeAdvanceObject qvec gf = AdvanceObject body
  where
    body = drawingCtx >>= \ctx -> 
           let v1   = runCF ctx qvec
               pf   = runCF ctx gf
           in return (v1,pf)



-- | 'emptyAdvanceObjectAU' : @ AdvanceObject @
--
-- Build an empty 'AdvanceObject'.
-- 
-- The 'emptyAdvanceObject' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, the answer vector generated is
-- the zero vector @(V2 0 0)@.
-- 
emptyAdvanceObject :: InterpretUnit u => AdvanceObject u
emptyAdvanceObject = makeAdvanceObject (pure $ V2 0 0) emptyLocGraphic


runAdvanceObject :: AdvanceObject u -> AdvGraphic u
runAdvanceObject (AdvanceObject mf) = promoteR1 $ \pt -> 
   (\(v1,pf) -> replaceAns v1 $ pf pt) <$> mf


--------------------------------------------------------------------------------
-- Combining AdvanceObjects



-- | Primitive combination.
-- 
-- Move second object by the advance vector of the first. Sum 
-- both advance vecots.
--
advplus :: Num u 
          => AdvanceObject u -> AdvanceObject u -> AdvanceObject u
advplus a b = AdvanceObject body
  where 
    body = drawingCtx >>= \ctx ->
           let (v0,pf0) = runCF ctx (getAdvanceObject a)
               (v1,pf1) = runCF ctx (getAdvanceObject b)
               pf       = \pt -> pf0 pt `oplus` pf1 (pt .+^ v0)
           in return (v0 ^+^ v1, pf)

-- Helper for list concatenation.
-- 
listcat :: InterpretUnit u 
        => (AdvanceObject u -> AdvanceObject u -> AdvanceObject u)
        -> [AdvanceObject u] -> AdvanceObject u
listcat _ []     = emptyAdvanceObject
listcat op (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (b:bs) = go (acc `op` b) bs



-- AdvanceObject does not have the same ability to be concatenated
-- as PosObject - all the advance vector says is \"where to go 
-- next\". Nothing in the AdvanceObject tracks the boundary so we
-- cannot implement the Concat classes.

infixr 6 `next`


-- | Draw the first AdvanceObject and use the advance vector to 
-- displace the second AdvanceObject.
--
-- The final answer is the sum of both advance vectors.
--
next :: Num u 
     => AdvanceObject u -> AdvanceObject u -> AdvanceObject u
next = advplus
  

-- | Concatenate the list of AdvanceObjects with 'next'.
--
nexts :: InterpretUnit u => [AdvanceObject u] -> AdvanceObject u
nexts = listcat next


-- | Combine the AdvanceObjects using the answer vector of the 
-- first object plus the separator to move the start of the second
-- object. 
--
nextSpace :: Num u 
          => Vec2 u -> AdvanceObject u -> AdvanceObject u -> AdvanceObject u
nextSpace sep a b = AdvanceObject body
  where 
    body = drawingCtx >>= \ctx ->
           let (v0,pf0) = runCF ctx (getAdvanceObject a)
               (v1,pf1) = runCF ctx (getAdvanceObject b)
               pf       = \pt -> pf0 pt `oplus` pf1 (displaceVec (sep ^+^ v0) pt)
           in return (v0 ^+^ sep ^+^ v1, pf)

-- | List version of 'nextSpace'.
--
evenspace :: InterpretUnit u 
          => Vec2 u -> [AdvanceObject u] -> AdvanceObject u
evenspace v = listcat (nextSpace v)



-- | Repeat the AdvanceObject @n@ times, each time moving @next@.
--
repeatnext :: InterpretUnit u => Int -> AdvanceObject u -> AdvanceObject u
repeatnext n = nexts . replicate n


-- | Concatenate the list of AdvanceObjects, going next and adding
-- the separator at each step.
--
punctuate :: InterpretUnit u 
          => AdvanceObject u -> [AdvanceObject u] -> AdvanceObject u
punctuate sep =  listcat (\a b -> a `next` sep `next` b)



-- | Render the supplied AdvanceObject, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: Num u => Vec2 u -> AdvanceObject u -> AdvanceObject u
advfill sv a = AdvanceObject body
  where 
    body = drawingCtx >>= \ctx ->
           let (_,pf) = runCF ctx (getAdvanceObject a) in return (sv, pf)



