{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.AdvNotelist
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ...
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Objects.AdvNotelist
  ( 

    AdvNotelist
  , runAdvNotelist
  , execAdvNotelist

  , aevent

  ) where

import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Objects.LocEvent
import ZSnd.Basic.Kernel.Objects.TraceLoc

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Data.Monoid


newtype AdvNotelist ctx u a = AdvNotelist 
          { getAdvNotelist :: Env -> TraceLoc ctx u a }


data Env = Env
      { staccato_factor :: Double       -- should be [0.. 1.0]
      } 

envZero :: Env
envZero = Env { staccato_factor = 1.0 }


-- staccato wants to be in an Env...


instance Functor (AdvNotelist ctx u) where
  fmap f mf = AdvNotelist $ \r -> fmap f (getAdvNotelist mf r)

instance Applicative (AdvNotelist ctx u) where
  pure a    = AdvNotelist $ \_ -> pure a
  mf <*> ma = AdvNotelist $ \r -> getAdvNotelist mf r <*> getAdvNotelist ma r

instance Monad (AdvNotelist ctx u) where
  return a  = AdvNotelist $ \_ -> return a
  ma >>= k  = AdvNotelist $ \r -> getAdvNotelist ma r >>= \a -> 
                                  getAdvNotelist (k a) r


instance Monoid a => Monoid (AdvNotelist ctx u a) where
  mempty           = AdvNotelist $ \_ -> return mempty
  ma `mappend` mb  = AdvNotelist $ \r -> 
                       getAdvNotelist ma r `mappend` getAdvNotelist mb r


runAdvNotelist :: Num u => AdvNotelist ctx u a -> (a, ULocEvent ctx u)
runAdvNotelist mf = post $ runTraceLoc (getAdvNotelist mf envZero) 
  where
    post (a,_,w) = (a,w)


execAdvNotelist :: Num u => AdvNotelist ctx u a -> ULocEvent ctx u
execAdvNotelist = snd . runAdvNotelist


-- | @aevent@ scales the playing duration automatically by the 
-- staccato factor and moves the cursor by the ful duration.
--
aevent :: (CtxTempo ctx, InterpretUnit u, VectorSpace u, Fractional (Scalar u)) 
       => (u -> ULocEvent ctx u) -> u -> AdvNotelist ctx u ()
aevent mf d = AdvNotelist $ \r -> 
    let d1 = d ^* (realToFrac $ staccato_factor r)
    in insertl (mf d1) >> moveBy d

                           
{-



-- | Promotion of @HPrim@ to @Picture@.
--
-- 
liftToScore :: HPrim u -> Score
liftToScore = score1 . hprimToList

-}