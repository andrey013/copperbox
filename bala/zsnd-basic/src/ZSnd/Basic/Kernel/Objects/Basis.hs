{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.Basis
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

module ZSnd.Basic.Kernel.Objects.Basis
  ( 
    PrimW(..)
  
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

import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Base.WrappedPrimitive

import Control.Applicative
import Data.Monoid


-- | Unit @u@ is a phantom - it is some unit of duration.
--
data PrimW u a = PrimW CatPrim a


instance Monoid a => Monoid (PrimW u a) where
  mempty = PrimW mempty mempty
  PrimW ca a `mappend` PrimW cb b = PrimW (ca `mappend` cb) (a `mappend` b)

instance Functor (PrimW  u) where
  fmap f (PrimW ca a) = PrimW ca (f a)


instance Applicative (PrimW u) where
  pure a                        = PrimW mempty a
  (PrimW c1 f) <*> (PrimW c2 a) = PrimW (c1 `mappend` c2) (f a) 


instance Monad (PrimW u) where
  return a            = PrimW mempty a
  (PrimW c1 a) >>= mf = let (PrimW c2 b) = mf a
                        in PrimW (c1 `mappend` c2) b




-- | Convert a PrimW where the answer is some functor type 
-- parametrized by the unit.
--
szconvPrimF :: (Functor t, InterpretUnit u, InterpretUnit u1)
            => Tempo -> PrimW u (t u)  -> PrimW u1 (t u1)
szconvPrimF bpm (PrimW c a) = PrimW c (uconvertF bpm a)

-- | Convert a PrimW where the answer is oblivious to unit.
--
szconvPrimZ :: (InterpretUnit u, InterpretUnit u1)
            => Tempo -> PrimW u a  -> PrimW u1 a
szconvPrimZ _ (PrimW c a) = PrimW c a






-- | For the moment the second fun is type preserving...
--

-- bimapPrimW :: (CatPrim -> CatPrim) -> (a -> a) -> PrimW u a -> PrimW u a
-- bimapPrimW f g (PrimW ca a) = PrimW (f ca) (g a)



newtype Event ctx u a = Event { 
          getEvent :: ctx -> PrimW u a }

type instance DUnit (Event ctx u a) = u
type instance Ctx (Event ctx u) = ctx


type DEvent ctx a = Event ctx Double a

type UEvent ctx u = Event ctx u (UNil u)


newtype Query ctx u a =  Query { getQuery :: ctx -> a }

type instance DUnit (Query ctx u a) = u
type instance Ctx (Query ctx u) = ctx

-- Functor

instance Functor (Event ctx u) where
  fmap f ma = Event $ \ctx -> fmap f $ getEvent ma ctx

instance Functor (Query ctx u) where
  fmap f ma = Query $ \ctx -> f $ getQuery ma ctx


-- Applicative

instance Applicative (Event ctx u) where
  pure a    = Event $ \_   -> pure a
  mf <*> ma = Event $ \ctx -> 
                getEvent mf ctx <*> getEvent ma ctx

instance Applicative (Query ctx u) where
  pure a    = Query $ \_   -> a
  mf <*> ma = Query $ \ctx -> let f = getQuery mf ctx 
                                  a = getQuery ma ctx
                              in f a

-- Monad

instance Monad (Event ctx u) where
  return a = Event $ \_   -> return a
  ma >>= k = Event $ \ctx -> getEvent ma ctx >>= \ans -> getEvent (k ans) ctx

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
  askCtx          = Event $ \ctx -> return ctx
  asksCtx fn      = Event $ \ctx -> return (fn ctx)
  localize upd ma = Event $ \ctx -> getEvent ma (upd ctx)

instance ContextM (Query ctx u) where
  askCtx          = Query $ \ctx -> ctx
  asksCtx fn      = Query $ \ctx -> fn ctx
  localize upd ma = Query $ \ctx -> getQuery ma (upd ctx)


runEvent :: ctx -> Event ctx u a -> PrimW u a
runEvent ctx mf = getEvent mf ctx


runQuery :: ctx -> Query ctx u a -> a
runQuery ctx mf = getQuery mf ctx


zapQuery :: Query ctx u a -> Event ctx u a
zapQuery mq = askCtx >>= \ctx -> let a = runQuery ctx mq in return a


-- | Constructor for Primtive graphics.
--
primEvent :: CatPrim -> UEvent ctx u
primEvent ca = Event $ \_ -> PrimW ca UNil



class UConvert (f :: * -> * -> * -> *) where
  uconvF :: (CtxTempo ctx, Functor t, InterpretUnit u, InterpretUnit u1) 
         => f ctx u (t u) -> f ctx u1 (t u1)

  uconvZ :: (CtxTempo ctx, InterpretUnit u, InterpretUnit u1) 
         => f ctx u a -> f ctx u1 a

instance UConvert Event where
  uconvZ = uconvEventZ
  uconvF = uconvEventF


uconvEventF :: (CtxTempo ctx, Functor t, InterpretUnit u, InterpretUnit u1) 
             => Event ctx u (t u) -> Event ctx u1 (t u1)
uconvEventF ma = Event $ \ctx -> 
    szconvPrimF (tempo ctx) $ getEvent ma ctx



uconvEventZ :: (CtxTempo ctx, InterpretUnit u, InterpretUnit u1) 
             => Event ctx u a -> Event ctx u1 a
uconvEventZ ma = Event $ \ctx -> 
    szconvPrimZ (tempo ctx) $ getEvent ma ctx




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
    let (PrimW ca a) = getEvent ma ctx
        (PrimW cb _) = getEvent mb ctx
    in PrimW (cb `mappend` ca) a


-- | Elaborate Event.
--
elaborateEvent :: Event ctx u a -> (a -> Event ctx u z) -> Event ctx u a
elaborateEvent ma k = Event $ \ ctx ->
    let (PrimW ca a) = getEvent ma ctx
        (PrimW cb _) = getEvent (k a) ctx
    in PrimW (ca `mappend` cb) a

-- | Obliterate Event.
--
obliterateEvent :: Event ctx u a -> Event ctx u z -> Event ctx u a
obliterateEvent ma mb = Event $ \ctx -> 
    let (PrimW _  a) = getEvent ma ctx
        (PrimW cb _) = getEvent mb ctx
    in PrimW cb a



