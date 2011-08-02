{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Objects.Connector
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Connector - functional type from start point to end point that
-- forms an event /primitive/.
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Objects.Connector
   (
     Connector
   , DConnector
   , UConnector

   , runConnector
   , connect

   , promoteConn
   , applyConn

   , emptyConnector

   )

   where

import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Objects.Basis

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Data.Monoid


-- | ConnectorImage - function from DrawingContext and start and 
-- end points to a polymorphic /answer/ and a graphic /primitive/.
--
newtype Connector ctx u a = Connector { 
          getConnector :: Double -> Double -> Event ctx u a }


type instance DUnit (Connector ctx u a) = u
type instance UCtx  (Connector ctx u)   = ctx

-- | Type specialized version of 'Connector'.
--
type DConnector ctx a        = Connector ctx Double a


type UConnector ctx u        = Connector ctx u (UNil u)




-- Functor 

instance Functor (Connector ctx u) where
  fmap f ma = Connector $ \t0 t1 -> fmap f $ getConnector ma t0 t1


-- Applicative

instance Applicative (Connector ctx u) where
  pure a    = Connector $ \_  _  -> pure a
  mf <*> ma = Connector $ \t0 t1 -> 
                getConnector mf t0 t1 <*> getConnector ma t0 t1

-- Monad 

instance Monad (Connector ctx u) where
  return a  = Connector $ \_  _  -> return a
  ma >>= k  = Connector $ \t0 t1 -> 
                getConnector ma t0 t1 >>= \ans -> 
                getConnector (k ans) t0 t1


-- Monoid

instance Monoid a => Monoid (Connector ctx u a) where
  mempty          = pure mempty
  ma `mappend` mb = Connector $ \t0 t1 -> 
                      getConnector ma t0 t1 
                        `mappend` getConnector mb t0 t1 


-- DrawingCtxM

instance ContextM (Connector ctx u) where
  askCtx          = Connector $ \_  _  -> askCtx
  asksCtx f       = Connector $ \_  _  -> asksCtx f
  localize upd ma = Connector $ \t0 t1 -> 
                      localize upd (getConnector ma t0 t1)

  


instance Decorate Connector where
  decorate ma mz = Connector $ \t0 t1 -> 
    decorate (getConnector ma t0 t1) (getConnector mz t0 t1)

  elaborate ma f = Connector $ \t0 t1 -> 
    elaborate (getConnector ma t0 t1) (\a -> getConnector (f a) t0 t1)

  obliterate ma mz = Connector $ \t0 t1 -> 
    getConnector ma t0 t1 `obliterate` getConnector mz t0 t1



runConnector :: InterpretUnit u
             => u -> u -> Context ctx -> Connector ctx u a 
             -> PrimW u a
runConnector t0 t1 ctx mf = 
    let dt0 = normalize (ctx_tempo ctx) t0
        dt1 = normalize (ctx_tempo ctx) t1 
    in runEvent ctx (getConnector mf dt0 dt1)


connect :: InterpretUnit u
        => u -> u -> Connector ctx u a -> Event ctx u a
connect t0 t1 mf = 
    normalizeCtx t0 >>= \dt0 -> 
    normalizeCtx t1 >>= \dt1 -> 
    getConnector mf dt0 dt1




promoteConn :: InterpretUnit u
            => (u -> u -> Event ctx u a) -> Connector ctx u a
promoteConn k = Connector $ \t0 t1 ->
    dinterpCtx t0 >>= \ut0 -> dinterpCtx t1 >>= \ut1 -> k ut0 ut1


applyConn :: InterpretUnit u 
          => Connector ctx u a -> u -> u -> Event ctx u a
applyConn mf t0 t1 = connect t0 t1 mf




instance UConvert Connector where
  uconvF = uconvConnectorF
  uconvZ = uconvConnectorZ


-- | Use this to convert 'ConnectorGraphic' or 'Connector' 
-- with Functor answer.
--
uconvConnectorF :: (InterpretUnit u, InterpretUnit u1, Functor t) 
                => Connector ctx u (t u) -> Connector ctx u1 (t u1)
uconvConnectorF ma = Connector $ \t0 t1 -> 
    uconvF $ getConnector ma t0 t1




-- | Use this to convert 'Connector' with unit-less answer.
--
uconvConnectorZ :: (InterpretUnit u, InterpretUnit u1) 
                => Connector ctx u a -> Connector ctx u1 a
uconvConnectorZ ma = Connector $ \t0 t1 -> 
    uconvZ $ getConnector ma t0 t1


-- | Having /empty/ at the specific 'Connector' type is useful.
-- 
emptyConnector :: Monoid a => Connector ctx u a
emptyConnector = mempty


--------------------------------------------------------------------------------



--
-- Design note - potentially there are no useful combining 
-- operators on Connectors (!).
--
-- Division - i.e. splitting a path at points between the start 
-- and end - seems a more obvious operation on connector paths 
-- than combination. 
--

