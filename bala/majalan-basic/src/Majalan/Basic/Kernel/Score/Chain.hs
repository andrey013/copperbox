{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Score.Chain
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- /Chained/ scored (/chain/ is the TikZ drawing object).
--
-- @Where-to-go-next@ is determined by the chain. Individual 
-- events simply /consume/ their position. 
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Score.Chain
  (

    Chain

  ) where




import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.LocEvent

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Control.Monad
import Data.Monoid


data ChainSt u = ChainSt { chain_pos :: !u, chain_next :: (u -> u) }


type instance DUnit (ChainSt u) = u


newtype Chain ctx u a = Chain
          { getChain :: ChainSt u -> (a, ChainSt u, ULocEvent ctx u) }



newtype ChainT ctx u m a = ChainT 
          { getChainT :: ChainSt u -> m (a, ChainSt u, ULocEvent ctx u) }


type instance DUnit (Chain ctx u a) = u
type instance UCtx  (Chain ctx u)   = ctx

type instance DUnit (ChainT ctx u m a) = u
type instance UCtx  (ChainT ctx u m)   = ctx

type DChain ctx a    = Chain ctx Double a
type DChainT ctx m a = ChainT ctx Double m a



-- Functor 

instance Functor (Chain ctx u) where
  fmap f ma = Chain $ \s0 -> let (a,s1,w) = getChain ma s0 
                                in (f a, s1, w)


instance Monad m => Functor (ChainT ctx u m) where
  fmap f ma = ChainT $ \s0 -> getChainT ma s0 >>= \(a,s1,w) -> 
                                 return (f a, s1, w)


-- Applicative

instance Applicative (Chain ctx u) where
  pure a    = Chain $ \s0 -> (a, s0, mempty)
  mf <*> ma = Chain $ \s0 -> 
                let (f,s1,w1) = getChain mf s0
                    (a,s2,w2) = getChain ma s1
                in (f a, s2, w1 `mappend` w2)


instance Monad m => Applicative (ChainT ctx u m) where
  pure a    = ChainT $ \s0 -> return (a, s0, mempty)
  mf <*> ma = ChainT $ \s0 -> 
                getChainT mf s0 >>= \(f,s1,w1) ->
                getChainT ma s1 >>= \(a,s2,w2) ->
                return (f a, s2, w1 `mappend` w2)


-- Monad

instance Monad (Chain ctx u) where
  return a  = Chain $ \s0 -> (a, s0, mempty)
  ma >>= k  = Chain $ \s0 -> 
                let (a,s1,w1) = getChain ma s0
                    (b,s2,w2) = (getChain . k) a s1
                in (b, s2, w1 `mappend` w2)



instance Monad m => Monad (ChainT ctx u m) where
  return a  = ChainT $ \s0 -> return (a, s0, mempty)
  ma >>= k  = ChainT $ \s0 -> 
                getChainT ma s0      >>= \(a,s1,w1) ->
                (getChainT . k) a s1 >>= \(b,s2,w2) -> 
                return (b, s2, w1 `mappend` w2)



-- No instances for ContextM

-- Monoid

instance Monoid a => Monoid (Chain ctx u a) where
  mempty           = Chain $ \s0 -> (mempty, s0, mempty)
  ma `mappend` mb  = Chain $ \s0 -> 
                       let (a,s1,w1) = getChain ma s0
                           (b,s2,w2) = getChain mb s1
                       in (a `mappend` b, s2, w1 `mappend` w2)



instance (Monad m, Monoid a) => Monoid (ChainT ctx u m a) where
  mempty           = ChainT $ \s0 -> return (mempty, s0, mempty)
  ma `mappend` mb  = ChainT $ \s0 -> 
                       getChainT ma s0 >>= \(a,s1,w1) ->
                       getChainT mb s1 >>= \(b,s2,w2) ->
                       return (a `mappend` b, s2, w1 `mappend` w2)

-- By default - chain starts at zero...
-- Actually this needs a some thought.


runChain :: Num u 
         => Chain ctx u a -> (a, u, ULocEvent ctx u)
runChain mf = post $ getChain mf (ChainSt 0 (+0))
  where
    post (a,s,w) = (a, chain_pos s, w)

{-


-- | Forget the generated LocEvent, just return the /answer/.
--
evalChain :: Num u => Chain ctx u a -> a
evalChain = post . runChain
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated ULocEvent.
--
execChain :: Num u => Chain ctx u a -> ULocEvent ctx u
execChain = post . runChain
  where
    post (_,_,o) = o




runChainT :: (Monad m, Num u) 
             => ChainT ctx u m a -> m (a, u, ULocEvent ctx u)
runChainT mf = getChainT mf 0


-- | Forget the generated LocImage, just return the /answer/.
--
evalChainT :: (Monad m, Num u) => ChainT ctx u m a -> m a
evalChainT = liftM post . runChainT
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated LocImage.
--
execChainT :: (Monad m, Num u) 
              => ChainT ctx u m a -> m (ULocEvent ctx u)
execChainT = liftM post . runChainT
  where
    post (_,_,o) = o




liftChainT :: Monad m => m a -> ChainT ctx u m a 
liftChainT ma = ChainT $ \s0 -> 
                     ma >>= \a -> return (a,s0,mempty)


--------------------------------------------------------------------------------
-- ChainM classes

-- | 'insertl' analogue to Writer monad @tell@.
--
class Monad m => ChainM (m :: * -> *) where
  insertl   :: (u ~ DUnit (m ()), ctx ~ UCtx m) 
            => ULocEvent ctx u -> m ()
  insertl_  :: (u ~ DUnit (m ()), ctx ~ UCtx m) 
            => LocEvent ctx u a -> m ()
  
  moveBy    :: u ~ DUnit (m ()) => u -> m ()
  location  :: u ~ DUnit (m ()) => m u


  insertl_ = insertl . ignoreAns 



-- Note - @reset@ steals a too general name. 
-- It needs changing...

-- | Add operations for branching (fork at the current point)
-- and resetting to the start point.
-- 
-- Not all drawings that support tracing support branching. For
-- instance Paths can be built by tracing but they always need 
-- a cumulative progression of /next point/ they cannot resrt to 
-- the start point and go in a differnt direction.
-- 
class ChainM m => LocForkTraceM (m :: * -> *) where
  reset     :: m ()

  -- Branch is like @local@ in the Reader monad.
  branch    :: m a -> m a




-- ChainM

instance InterpretUnit u => ChainM (Chain ctx u) where
  insertl gf  = Chain $ \s0 -> ((), s0, moveStart s0 gf)
  moveBy v    = Chain $ \s0 -> ((), s0 + v, mempty)
  location    = Chain $ \s0 -> (s0, s0, mempty)


instance InterpretUnit u => LocForkTraceM (Chain ctx u) where
  reset       = Chain $ \_  -> ((), 0, mempty)
  branch ma   = Chain $ \s0 -> 
                  let (a,_,o) = getChain ma s0 in (a,s0,o)



instance (Monad m, InterpretUnit u) => 
    ChainM (ChainT ctx u m) where
  insertl gf  = ChainT $ \s0 -> 
                  return ((), s0, moveStart s0 gf)
  moveBy v    = ChainT $ \s0 -> return ((), s0 + v, mempty)
  location    = ChainT $ \s0 -> return (s0, s0, mempty)

instance (ChainM m, InterpretUnit u) => 
    LocForkTraceM (ChainT ctx u m) where
  reset       = ChainT $ \_  -> return ((), 0, mempty)
  branch ma   = ChainT $ \s0 -> getChainT ma s0 >>= \(a,_,o) -> 
                                   return (a,s0,o)


-}