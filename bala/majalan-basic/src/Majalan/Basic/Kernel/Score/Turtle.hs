{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Score.Turtle
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Turtle (or /Path-like/) score object.
--
-- Choice of @where-to-go-next@ is determined individually at each
-- event.
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Score.Turtle
  (
  
    Turtle
  , DTurtle
  
  , runTurtle
  , runTurtle_
  , renderTurtle
  , renderTurtleU

  , insertl
  , insertl_
  , moveBy
  , location
  , reset
  , branch

  ) where


import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.LocEvent

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Data.Monoid


-- Want to run a Turtle and get a ULocEvent 


newtype Turtle ctx u a = Turtle
          { getTurtle :: Context ctx -> OnsetDbl -> (a, OnsetDbl, CatPrim) }



type instance DUnit (Turtle ctx u a) = u
type instance UCtx  (Turtle ctx u)   = ctx

type DTurtle ctx a    = Turtle ctx Double a



-- Functor 

instance Functor (Turtle ctx u) where
  fmap f ma = Turtle $ \r s -> let (a,s1,w) = getTurtle ma r s 
                               in (f a,s1,w)


-- Applicative

instance Applicative (Turtle ctx u) where
  pure a    = Turtle $ \_ s -> (a, s, mempty)
  mf <*> ma = Turtle $ \r s -> 
                let (f, s1, w1) = getTurtle mf r s
                    (a, s2, w2) = getTurtle ma r s1
                in (f a, s2, w1 `mappend` w2)


-- Monad

instance Monad (Turtle ctx u) where
  return a  = Turtle $ \_ s -> (a, s, mempty) 
  ma >>= k  = Turtle $ \r s -> 
                let (a, s1, w1) = getTurtle ma r s
                    (b, s2, w2) = (getTurtle . k) a r s1
                in (b, s2, w1 `mappend` w2)


instance ContextM (Turtle ctx u) where
  askCtx          = Turtle $ \r s -> (r, s, mempty)
  asksCtx fn      = Turtle $ \r s -> (fn r, s, mempty)
  localize upd ma = Turtle $ \r s -> getTurtle ma (upd r) s



-- Monoid

instance Monoid a => Monoid (Turtle ctx u a) where
  mempty           = Turtle $ \_ s -> (mempty, s, mempty)
  ma `mappend` mb  = Turtle $ \r s -> 
                       let (a,s1,w1) = getTurtle ma r s
                           (b,s2,w2) = getTurtle mb r s1
                       in (a `mappend` b, s2, w1 `mappend` w2)



runTurtle :: InterpretUnit u => Turtle ctx u a -> LocEvent ctx u a
runTurtle mf = promoteLoc $ \ot -> 
                 askCtx  >>= \ctx ->
                 normalizeCtx ot >>= \dot -> 
                 let (a, _, ca) = getTurtle mf ctx dot 
                     evt        = primEvent ca
                 in replaceAns a $ evt

runTurtle_ :: InterpretUnit u => Turtle ctx u a -> ULocEvent ctx u
runTurtle_ mf = promoteLoc $ \ot -> 
                 askCtx  >>= \ctx ->
                 normalizeCtx ot >>= \dot -> 
                 let (_, _, ca) = getTurtle mf ctx dot 
                 in primEvent ca


renderTurtle :: InterpretUnit u 
             => Context ctx -> Turtle ctx u a -> Maybe RScore
renderTurtle ctx mf = 
   let PrimW ca _ = runEvent ctx (applyLoc (runTurtle mf) 0)
   in hprimToScoreMb $ singleH ca


renderTurtleU :: InterpretUnit u 
             => Context ctx -> Turtle ctx u a -> RScore
renderTurtleU ctx mf = maybe fk id $ renderTurtle ctx mf
  where
    fk = error "renderTurtleU - empty score." 




-- Note - there is no transformer version as I don\'t think there
-- is a sensible run function for a transformer (Wumpus has a 
-- transfomer, but potentially Wumpus is wrong). 
--
-- The circumstance is the same as IO, LocEvent should always be 
-- the /outer/ monad.


insertl :: InterpretUnit u => LocEvent ctx u a -> Turtle ctx u a
insertl gf  = Turtle $ \r s -> 
    let sx         = dinterp (ctx_tempo r) s
        PrimW ca a = runEvent r (applyLoc gf sx)
    in (a, s, ca)

insertl_  :: InterpretUnit u
          => LocEvent ctx u a -> Turtle ctx u ()
insertl_ gf  = Turtle $ \r s -> 
    let sx         = dinterp (ctx_tempo r) s
        PrimW ca _ = runEvent r (applyLoc gf sx)
    in ((), s, ca)

  
moveBy :: InterpretUnit u => u -> Turtle ctx u ()
moveBy v = Turtle $ \r s -> 
    let dv = normalize (ctx_tempo r) v in ((), s + dv, mempty)

location :: InterpretUnit u => Turtle ctx u u
location = Turtle $ \r s -> 
    let sx = dinterp (ctx_tempo r) s in (sx, s, mempty)

reset :: InterpretUnit u => Turtle ctx u ()
reset = Turtle $ \_ _ -> ((), 0, mempty)


branch :: InterpretUnit u => Turtle ctx u a -> Turtle ctx u a
branch ma = Turtle $ \r s -> 
    let (a,_,o) = getTurtle ma r s in (a,s,o)

