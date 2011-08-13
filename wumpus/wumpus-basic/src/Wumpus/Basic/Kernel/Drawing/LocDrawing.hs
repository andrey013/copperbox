{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Drawing.LocDrawing
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Drawing monad with immutable start point.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Drawing.LocDrawing
  (

  -- * GenLocDrawing monad
    GenLocDrawing
  , LocDrawing

  , LocDrawM(..)

  , runGenLocDrawing
  , evalGenLocDrawing
  , execGenLocDrawing
  , stripGenLocDrawing

  , runLocDrawing
  , runLocDrawing_ 

  )

  where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Drawing.Basis
import Wumpus.Basic.Kernel.Objects.Anchors
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid




-- | 'GenLocDrawing' is a reader-writer-state monad, unlike 
-- 'GenLocTrace' there is no updateable current point, instead 
-- the point is an immutable /origin/.
--
-- The writer accumulates a graphical trace.
--
-- Essentially, 'GenLocDrawing' is a 'LocImage' object extended 
-- with user state.
--
newtype GenLocDrawing st u a = GenLocDrawing { 
    getGenLocDrawing :: DrawingContext -> DPoint2 -> st -> (a, st, CatPrim)}

type instance DUnit  (GenLocDrawing st u a) = u
type instance UState (GenLocDrawing st u)   = st

type LocDrawing u a = GenLocDrawing () u a


-- Functor

instance Functor (GenLocDrawing st u) where
  fmap f ma = GenLocDrawing $ \ctx pt s -> 
    let (a,s1,o) = getGenLocDrawing ma ctx pt s in (f a, s1, o)


-- Applicative

instance Applicative (GenLocDrawing st u) where
  pure a    = GenLocDrawing $ \_   _  s -> (a, s, mempty)
  mf <*> ma = GenLocDrawing $ \ctx pt s -> 
                let (f,s1,o1) = getGenLocDrawing mf ctx pt s
                    (a,s2,o2) = getGenLocDrawing ma ctx pt s1
                in (f a, s2, o1 `mappend` o2)



-- Monad

instance Monad (GenLocDrawing st u) where
  return a  = GenLocDrawing $ \_   _  s -> (a, s, mempty)
  ma >>= k  = GenLocDrawing $ \ctx pt s -> 
                let (a,s1,o1) = getGenLocDrawing ma ctx pt s
                    (b,s2,o2) = (getGenLocDrawing . k) a ctx pt s1
                in (b, s2, o1 `mappend` o2)



-- DrawingCtxM

instance DrawingCtxM (GenLocDrawing st u) where
  askDC           = GenLocDrawing $ \ctx _  s -> (ctx, s, mempty)
  asksDC fn       = GenLocDrawing $ \ctx _  s -> (fn ctx, s, mempty)
  localize upd ma = GenLocDrawing $ \ctx pt s -> 
                      getGenLocDrawing ma (upd ctx) pt s



-- UserStateM 

instance UserStateM (GenLocDrawing st u) where
  getState        = GenLocDrawing $ \_ _ s -> (s, s, mempty)
  setState s      = GenLocDrawing $ \_ _ _ -> ((), s, mempty)
  updateState upd = GenLocDrawing $ \_ _ s -> ((), upd s, mempty)


-- Monoid

instance Monoid a => Monoid (GenLocDrawing st u a) where
  mempty           = GenLocDrawing $ \_   _ s -> (mempty, s, mempty)
  ma `mappend` mb  = GenLocDrawing $ \ctx pt s -> 
                       let (a,s1,w1) = getGenLocDrawing ma ctx pt s
                           (b,s2,w2) = getGenLocDrawing mb ctx pt s1
                       in (a `mappend` b, s2, w1 `mappend` w2)



instance InterpretUnit u => InsertlM (GenLocDrawing st u) where
  insertl   = insertlImpl




--------------------------------------------------------------------------------

class LocDrawM (m :: * -> *) where
  inserti  :: (Translate a, u ~ DUnit a, u ~ DUnit (m ())) => Image u a -> m a
  insertli :: u ~ DUnit (m ()) => Anchor u -> LocImage u a -> m a
  insertci :: u ~ DUnit (m ()) => 
              Anchor u -> Anchor u -> ConnectorImage u a -> m a


instance InterpretUnit u => LocDrawM (GenLocDrawing st u) where
  inserti  = insertiImpl
  insertli = insertliImpl
  insertci = insertciImpl

 
--------------------------------------------------------------------------------
-- Run functions


runGenLocDrawing :: InterpretUnit u 
               => GenLocDrawing st u a -> st -> LocImage u (a,st)
runGenLocDrawing ma st = promoteLoc $ \pt -> 
    askDC >>= \ctx ->
    let dpt       = normalizeF (dc_font_size ctx) pt
        (a,s1,w1) = getGenLocDrawing ma ctx dpt st
    in replaceAns (a,s1) $ primGraphic w1




-- | Forget the user state LocImage, just return the /answer/.
--
evalGenLocDrawing :: InterpretUnit u 
                => GenLocDrawing st u a -> st -> LocImage u a
evalGenLocDrawing ma st = fmap fst $ runGenLocDrawing ma st


-- | Forget the /answer/, just return the user state.
--
execGenLocDrawing :: InterpretUnit u 
                => GenLocDrawing st u a -> st -> LocImage u st 
execGenLocDrawing ma st = fmap snd $ runGenLocDrawing ma st


stripGenLocDrawing :: InterpretUnit u 
                 => GenLocDrawing st u a -> st -> LocQuery u (a,st)
stripGenLocDrawing ma st = stripLocImage $ runGenLocDrawing ma st 


-- | Simple version of 'runGenLocDrawing' - run a 'LocDrawing' without
-- user state.
--
runLocDrawing :: InterpretUnit u 
            => LocDrawing u a -> LocImage u a
runLocDrawing ma = evalGenLocDrawing ma ()


runLocDrawing_ :: InterpretUnit u 
             => LocDrawing u a -> LocGraphic u 
runLocDrawing_ ma = ignoreAns $ runLocDrawing ma



--------------------------------------------------------------------------------

insertiImpl :: (InterpretUnit u, Translate a, u ~ DUnit a) 
            => Image u a -> GenLocDrawing st u a
insertiImpl gf = GenLocDrawing $ \ctx pt s -> 
    let (P2 x y) = dinterpF (dc_font_size ctx) pt 
        (a,w1)   = runImage (translate x y gf) ctx
    in (a,s,w1) 



insertlImpl :: InterpretUnit u 
            => LocImage u a -> GenLocDrawing st u a
insertlImpl gf = GenLocDrawing $ \ctx pt s -> 
    let upt    = dinterpF (dc_font_size ctx) pt 
        (a,w1) = runLocImage gf ctx upt
    in (a,s,w1) 




-- | Helper - change points to vectors.
-- 
vecPt :: Point2 u -> Vec2 u
vecPt (P2 x y) = V2 x y


insertliImpl :: InterpretUnit u
             => Anchor u -> LocImage u a -> GenLocDrawing st u a
insertliImpl p1 gf = GenLocDrawing $ \ctx pt s -> 
    let upt    = dinterpF (dc_font_size ctx) pt 
        (a,w1) = runLocImage gf ctx (upt .+^ vecPt p1) 
    in (a,s,w1) 




insertciImpl :: InterpretUnit u 
             => Anchor u -> Anchor u -> ConnectorImage u a 
             -> GenLocDrawing st u a
insertciImpl p1 p2 gf = GenLocDrawing $ \ctx pt s -> 
    let upt    = dinterpF (dc_font_size ctx) pt
        (a,w1) = runConnectorImage gf ctx (upt .+^ vecPt p1) (upt .+^ vecPt p2)
    in (a,s,w1) 
