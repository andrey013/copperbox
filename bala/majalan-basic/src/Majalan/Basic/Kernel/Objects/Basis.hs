{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Objects.Basis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- 
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Objects.Basis
  ( 
  
    PrimResult
  
  , Event
  , DEvent
  , UEvent

  , Query

  , runEvent
  , runQuery
  , zapQuery

  , primEvent

  , UConvert(..)

  , emptyEvent

  , ignoreAns
  , replaceAns

  , Decorate(..)

  ) where

import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Data.Monoid


type PrimResult u a = (a,CatPrim)

-- Note - instrument numbering is expected to be provided by the
-- user-context.
--
newtype Event ctx u a = Event { 
          getEvent :: Context ctx -> (a, CatPrim) }

type instance DUnit (Event ctx u a) = u
type instance UCtx  (Event ctx u)   = ctx


type DEvent ctx a = Event ctx Double a

type UEvent ctx u = Event ctx u (UNil u)

-- Note Query might not need...

newtype Query ctx u a =  Query { getQuery :: Context ctx -> a }

type instance DUnit (Query ctx u a) = u
type instance UCtx  (Query ctx u)   = ctx

-- Functor

instance Functor (Event ctx u) where
  fmap f ma = Event $ \ctx -> let (a,w1) = getEvent ma ctx in (f a, w1)

instance Functor (Query ctx u) where
  fmap f ma = Query $ \ctx -> f $ getQuery ma ctx


-- Applicative

instance Applicative (Event ctx u) where
  pure a    = Event $ \_   -> (a,mempty)
  mf <*> ma = Event $ \ctx -> let (f,w1) = getEvent mf ctx 
                                  (a,w2) = getEvent ma ctx
                              in (f a, w1 `mappend` w2)
              

instance Applicative (Query ctx u) where
  pure a    = Query $ \_   -> a
  mf <*> ma = Query $ \ctx -> let f = getQuery mf ctx 
                                  a = getQuery ma ctx
                              in f a

-- Monad

instance Monad (Event ctx u) where
  return a = Event $ \_   -> (a, mempty)
  ma >>= k = Event $ \ctx -> let (a,w1) = getEvent ma ctx 
                                 (b,w2) = getEvent (k a) ctx
                             in (b, w1 `mappend` w2)

instance Monad (Query ctx u) where
  return a = Query $ \_   -> a
  ma >>= k = Query $ \ctx -> let a = getQuery ma ctx 
                             in getQuery (k a) ctx


-- Monoid

instance Monoid a => Monoid (Event ctx u a) where
  mempty          = Event $ \_   -> mempty
  ma `mappend` mb = Event $ \ctx -> 
                      getEvent ma ctx `mappend` getEvent mb ctx

instance Monoid a => Monoid (Query ctx u a) where
  mempty          = Query $ \_   -> mempty
  ma `mappend` mb = Query $ \ctx -> 
                      getQuery ma ctx `mappend` getQuery mb ctx


-- RenderCtxM


instance ContextM (Event ctx u) where
  askCtx          = Event $ \ctx -> (ctx, mempty)
  asksCtx fn      = Event $ \ctx -> (fn ctx, mempty)
  localize upd ma = Event $ \ctx -> getEvent ma (upd ctx)

instance ContextM (Query ctx u) where
  askCtx          = Query $ \ctx -> ctx
  asksCtx fn      = Query $ \ctx -> fn ctx
  localize upd ma = Query $ \ctx -> getQuery ma (upd ctx)


runEvent :: Context ctx -> Event ctx u a -> (a,CatPrim)
runEvent ctx mf = getEvent mf ctx


runQuery :: Context ctx -> Query ctx u a -> a
runQuery ctx mf = getQuery mf ctx


-- | Run a Query building an Event.
--
zapQuery :: Query ctx u a -> Event ctx u a
zapQuery mq = askCtx >>= \ctx -> let a = runQuery ctx mq in return a


-- | Constructor for Primtive graphics.
--
primEvent :: CatPrim -> UEvent ctx u
primEvent ca = Event $ \_ -> (UNil, ca)



class UConvert (f :: * -> * -> * -> *) where
  uconvF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
         => f ctx u (t u) -> f ctx u1 (t u1)

  uconvZ :: (InterpretUnit u, InterpretUnit u1) 
         => f ctx u a -> f ctx u1 a

instance UConvert Event where
  uconvZ = uconvEventZ
  uconvF = uconvEventF


uconvEventF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
             => Event ctx u (t u) -> Event ctx u1 (t u1)
uconvEventF ma = Event $ \ctx -> 
    let (t,w1) = getEvent ma ctx in (uconvertF (ctx_tempo ctx) t, w1)



uconvEventZ :: (InterpretUnit u, InterpretUnit u1) 
             => Event ctx u a -> Event ctx u1 a
uconvEventZ ma = Event $ \ctx -> getEvent ma ctx




-- | Having /empty/ at the specific 'Event' type is useful.
-- 
emptyEvent :: Monoid a => Event ctx u a
emptyEvent = mempty



-- | Ignore an answer changing it to @()@.
--
ignoreAns :: Functor f => f a -> f (UNil u)
ignoreAns = fmap (const UNil)

-- | Replace the answer produced by a graphic object.
--
replaceAns :: Functor f => a -> f z -> f a
replaceAns a = fmap (const a)



-- | Decorate an object
--
-- oliterate - drops the output (notes) from the first object 
-- replacing it with the output from the second.
--
class Decorate (f :: * -> * -> * -> *) where
  decorate   :: f ctx u a -> f ctx u z -> f ctx u a
  elaborate  :: f ctx u a -> (a -> f ctx u z) -> f ctx u a
  obliterate :: f ctx u a -> f ctx u z -> f ctx u a



instance Decorate Event where
  decorate      = decorateEvent
  elaborate     = elaborateEvent
  obliterate    = obliterateEvent

-- | Decorate Event.
--
decorateEvent :: Event ctx u a -> Event ctx u z -> Event ctx u a
decorateEvent ma mb = Event $ \ctx -> 
    let (a,w1) = getEvent ma ctx
        (_,w2) = getEvent mb ctx
    in (a, w1 `mappend` w2)


-- | Elaborate Event.
--
elaborateEvent :: Event ctx u a -> (a -> Event ctx u z) 
               -> Event ctx u a
elaborateEvent ma k = Event $ \ ctx ->
    let (a,w1) = getEvent ma ctx
        (_,w2) = getEvent (k a) ctx
    in (a, w1 `mappend` w2)

-- | Obliterate Event.
--
obliterateEvent :: Event ctx u a -> Event ctx u z -> Event ctx u a
obliterateEvent ma mb = Event $ \ctx -> 
    let (a,_) = getEvent ma ctx
        (_,w) = getEvent mb ctx
    in (a,w)



