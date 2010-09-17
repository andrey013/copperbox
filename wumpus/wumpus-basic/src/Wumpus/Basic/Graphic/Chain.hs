{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Chain
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Supply points in an iterated chain.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Chain
  (

    ChainT
  , runChainT
  , horizontal

  ) where

import Wumpus.Basic.Graphic.BaseClasses
import Wumpus.Basic.Graphic.BaseTypes

import Wumpus.Core                              -- package: wumpus-core
       
import Control.Applicative
import Control.Monad



newtype ChainT u m a = ChainT { 
          getChainT :: Point2F u -> Point2 u -> m (a, Point2 u) }


type instance MonUnit (ChainT u m) = u



instance Monad m => Functor (ChainT u m) where
  fmap f ma = ChainT $ \rf s -> 
                getChainT ma rf s >>= \(a,s1) -> return (f a, s1)


instance Monad m => Applicative (ChainT u m) where
  pure a    = ChainT $ \_  s -> return (a,s)
  mf <*> ma = ChainT $ \rf s -> getChainT mf rf s  >>= \(f,s1) ->
                                getChainT ma rf s1 >>= \(a,s2) ->
                                return (f a, s2)


instance Monad m => Monad (ChainT u m) where
  return a = ChainT $ \_  s -> return (a,s)
  m >>= k  = ChainT $ \rf s -> getChainT m rf s >>= \(a,s1) ->
                               (getChainT . k) a rf s1
                                     


instance Monad m => PointSupplyM (ChainT u m) where
  position = ChainT $ \rf s -> return (s, rf s)

runChainT :: Monad m 
          => Point2F u -> Point2 u -> ChainT u m a -> m a
runChainT f pt0 ma = liftM fst $ getChainT ma f pt0

horizontal :: (Num u, Monad m)
           => Point2 u -> u -> ChainT u m a -> m a
horizontal pt0 dx ma = runChainT f pt0 ma
  where
    f (P2 x y) = P2 (x+dx) y


instance (Monad m, TraceM m, u ~ MonUnit m) => TraceM (ChainT u m) where
  trace a = ChainT $ \_ s -> trace a >> return ((), s)


instance (Monad m, DrawingCtxM m) => DrawingCtxM (ChainT u m) where
  askCtx         = ChainT $ \_ s -> askCtx >>= \ctx -> return (ctx, s)
  localCtx cF ma = ChainT $ \r s -> localCtx cF (getChainT ma r s)
