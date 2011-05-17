{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.AdvObject
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

module Wumpus.Basic.Kernel.Objects.AdvObject
  (


  -- * Advance vector
    AdvanceVec
  , advanceH
  , advanceV


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

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Monoid




--------------------------------------------------------------------------------

-- | Advance vectors provide an idiom for drawing consecutive
-- graphics. PostScript uses them to draw left-to-right text - 
-- each character has an advance vector for the width and 
-- as characters are drawn they successively displace the start
-- point for the next character with their advance vector.
--
-- Type alias for Vec2.
--
type AdvanceVec u = Vec2 u


-- | Extract the horizontal component of an advance vector.
--
-- For left-to-right latin text, the vertical component of an
-- advance vector is expected to be 0. Ingoring it seems 
-- permissible when drawing text.
--
advanceH :: AdvanceVec u -> u
advanceH (V2 w _)  = w

-- | Extract the verticall component of an advance vector.
--
advanceV :: AdvanceVec u -> u
advanceV (V2 _ h)  = h


--------------------------------------------------------------------------------
-- AdvObject

type AdvDraw u = Point2 u -> CatPrim



-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- advance (width) vector as each character is drawn.
--
newtype AdvObject u = AdvObject { getAdvObject :: Query u (Vec2 u, AdvDraw u) }

type instance DUnit (AdvObject u) = u

type DAdvObject     = AdvObject Double




--------------------------------------------------------------------------------

instance (InterpretUnit u) => Monoid (AdvObject u) where
  mempty  = blankAdvObject (V2 0 0)
  mappend = advplus


-- | Run an 'AdvObject' turning it into an 'LocImage'.
--
runAdvObject :: AdvObject u -> LocImage u (Vec2 u)
runAdvObject (AdvObject mf) = promoteLoc $ \pt -> 
   askDC >>= \ctx -> 
   let (v1,df) = runQuery ctx mf
   in replaceAns v1 $ primGraphic (df pt)


-- | 'makeAdvObject' : @ loc_context_function * graphic -> AdvObject @
--
-- Build an 'AdvObject' from a context function ('CF') that 
-- generates the answer displacement vector and a 'LocGraphic' 
-- that draws the 'AdvObject'.
--
makeAdvObject :: Query u (Vec2 u) -> LocGraphic u -> AdvObject u
makeAdvObject mq gf = AdvObject body
  where
    body = askDC >>= \ctx -> 
           let v1   = runQuery ctx mq
               pf   = \pt -> getCP $ runLocImage pt ctx gf
           in return (v1,pf)

    getCP (PrimW ca _) = ca


-- | 'emptyAdvObjectAU' : @ AdvObject @
--
-- Build an empty 'AdvObject'.
-- 
-- The 'emptyAdvObject' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, the answer vector generated is
-- the zero vector @(V2 0 0)@.
-- 
emptyAdvObject :: InterpretUnit u => AdvObject u
emptyAdvObject = blankAdvObject (V2 0 0)


blankAdvObject :: Vec2 u -> AdvObject u
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
        => (Vec2 u, AdvDraw u) 
        -> (Vec2 u, AdvDraw u) 
        -> (Vec2 u, AdvDraw u)
appendW (v0,pf0) (v1,pf1) = let pf = \pt -> pf0 pt `mappend` pf1 (pt .+^ v0)
                            in (v0 ^+^ v1, pf)


-- | Primitive combination.
-- 
-- Move second object by the advance vector of the first. Sum 
-- both advance vectors.
--
advplus :: Num u => AdvObject u -> AdvObject u -> AdvObject u
advplus a b = AdvObject body
  where 
    body = askDC >>= \ctx ->
           let ans1 = runQuery ctx (getAdvObject a)
               ans2 = runQuery ctx (getAdvObject b)
           in return (appendW ans1 ans2)




-- Helper for list concatenation.
-- 
listcat :: InterpretUnit u 
        => (AdvObject u -> AdvObject u -> AdvObject u)
        -> [AdvObject u] -> AdvObject u
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
advance :: Num u => AdvObject u -> AdvObject u -> AdvObject u
advance = advplus
  

-- | Concatenate the list of AdvObjects with 'advance'.
--
advances :: InterpretUnit u => [AdvObject u] -> AdvObject u
advances = listcat advance


-- | Combine the AdvObjects using the answer vector of the 
-- first object plus the separator to move the start of the second
-- object. 
--
advspace :: Num u => Vec2 u -> AdvObject u -> AdvObject u -> AdvObject u
advspace sep a b = a `advplus` blank `advplus` b
  where
    blank = blankAdvObject sep

-- | List version of 'nextSpace'.
--
evenspace :: InterpretUnit u => Vec2 u -> [AdvObject u] -> AdvObject u
evenspace v = listcat (advspace v)



-- | Repeat the AdvObject @n@ times, moving each time with 
-- 'advance'.
--
advrepeat :: InterpretUnit u => Int -> AdvObject u -> AdvObject u
advrepeat n = advances . replicate n


-- | Concatenate the list of AdvObjects, going next and adding
-- the separator at each step.
--
punctuate :: InterpretUnit u => AdvObject u -> [AdvObject u] -> AdvObject u
punctuate sep =  listcat (\a b -> a `advance` sep `advance` b)



-- | Render the supplied AdvObject, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: Num u => Vec2 u -> AdvObject u -> AdvObject u
advfill sv a = AdvObject body
  where 
    body = askDC >>= \ctx ->
           let (_,df) = runQuery ctx (getAdvObject a)
           in return (sv,df)


