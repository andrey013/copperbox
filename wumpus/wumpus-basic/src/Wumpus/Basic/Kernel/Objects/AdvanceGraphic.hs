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
    AdvObject
  , DAdvObject
  
  , AdvGraphic
  , DAdvGraphic

  , makeAdvObject
  , emptyAdvObject

  , runAdvObject

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
newtype AdvObject u = AdvObject { getAdvObject :: CF (Vec2 u, AdvDraw u) }

type instance DUnit (AdvObject u) = u

type DAdvObject     = AdvObject Double


type AdvGraphic u       = LocImage u (Vec2 u)
type DAdvGraphic        = AdvGraphic Double


--------------------------------------------------------------------------------

instance (InterpretUnit u) => Monoid (AdvObject u) where
  mempty  = emptyAdvObject
  mappend = advplus


-- | 'makeAdvObject' : @ loc_context_function * graphic -> AdvObject @
--
-- Build an 'AdvObject' from a context function ('CF') that 
-- generates the answer displacement vector and a 'LocGraphic' 
-- that draws the 'AdvObject'.
--
makeAdvObject :: Query (Vec2 u) -> LocGraphic u -> AdvObject u
makeAdvObject qvec gf = AdvObject body
  where
    body = drawingCtx >>= \ctx -> 
           let v1   = runCF ctx qvec
               pf   = runCF ctx gf
           in return (v1,pf)



-- | 'emptyAdvObjectAU' : @ AdvObject @
--
-- Build an empty 'AdvObject'.
-- 
-- The 'emptyAdvObject' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, the answer vector generated is
-- the zero vector @(V2 0 0)@.
-- 
emptyAdvObject :: InterpretUnit u => AdvObject u
emptyAdvObject = makeAdvObject (pure $ V2 0 0) emptyLocGraphic


runAdvObject :: AdvObject u -> AdvGraphic u
runAdvObject (AdvObject mf) = promoteR1 $ \pt -> 
   (\(v1,pf) -> replaceAns v1 $ pf pt) <$> mf


--------------------------------------------------------------------------------
-- Combining AdvObjects



-- | Primitive combination.
-- 
-- Move second object by the advance vector of the first. Sum 
-- both advance vecots.
--
advplus :: Num u => AdvObject u -> AdvObject u -> AdvObject u
advplus a b = AdvObject body
  where 
    body = drawingCtx >>= \ctx ->
           let (v0,pf0) = runCF ctx (getAdvObject a)
               (v1,pf1) = runCF ctx (getAdvObject b)
               pf       = \pt -> pf0 pt `oplus` pf1 (pt .+^ v0)
           in return (v0 ^+^ v1, pf)

-- Helper for list concatenation.
-- 
listcat :: InterpretUnit u 
        => (AdvObject u -> AdvObject u -> AdvObject u)
        -> [AdvObject u] -> AdvObject u
listcat _ []     = emptyAdvObject
listcat op (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (b:bs) = go (acc `op` b) bs



-- AdvObject does not have the same ability to be concatenated
-- as PosObject - all the advance vector says is \"where to go 
-- next\". Nothing in the AdvObject tracks the boundary so we
-- cannot implement the Concat classes.

infixr 6 `next`


-- | Draw the first AdvObject and use the advance vector to 
-- displace the second AdvObject.
--
-- The final answer is the sum of both advance vectors.
--
next :: Num u => AdvObject u -> AdvObject u -> AdvObject u
next = advplus
  

-- | Concatenate the list of AdvObjects with 'next'.
--
nexts :: InterpretUnit u => [AdvObject u] -> AdvObject u
nexts = listcat next


-- | Combine the AdvObjects using the answer vector of the 
-- first object plus the separator to move the start of the second
-- object. 
--
nextSpace :: Num u => Vec2 u -> AdvObject u -> AdvObject u -> AdvObject u
nextSpace sep a b = AdvObject body
  where 
    body = drawingCtx >>= \ctx ->
           let (v0,pf0) = runCF ctx (getAdvObject a)
               (v1,pf1) = runCF ctx (getAdvObject b)
               pf       = \pt -> pf0 pt `oplus` pf1 (displaceVec (sep ^+^ v0) pt)
           in return (v0 ^+^ sep ^+^ v1, pf)

-- | List version of 'nextSpace'.
--
evenspace :: InterpretUnit u => Vec2 u -> [AdvObject u] -> AdvObject u
evenspace v = listcat (nextSpace v)



-- | Repeat the AdvObject @n@ times, each time moving @next@.
--
repeatnext :: InterpretUnit u => Int -> AdvObject u -> AdvObject u
repeatnext n = nexts . replicate n


-- | Concatenate the list of AdvObjects, going next and adding
-- the separator at each step.
--
punctuate :: InterpretUnit u => AdvObject u -> [AdvObject u] -> AdvObject u
punctuate sep =  listcat (\a b -> a `next` sep `next` b)



-- | Render the supplied AdvObject, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: Num u => Vec2 u -> AdvObject u -> AdvObject u
advfill sv a = AdvObject body
  where 
    body = drawingCtx >>= \ctx ->
           let (_,pf) = runCF ctx (getAdvObject a) in return (sv, pf)



