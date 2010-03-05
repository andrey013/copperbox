{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.StateMap
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- A stateful variant of Functor/fmap
--
-- Experiment to see if it has any convenience over fmapM & a 
-- state monad. 
--
--------------------------------------------------------------------------------


module Neume.Core.Utils.StateMap 
  ( 
  -- * Stateful map
    StateMap(..)
  , StateMap2(..)
  , StateMap3(..)

  , stmap2a
  , stmap2b

  , stmap3a 
  , stmap3b
  , stmap3c

  ) where 

import Neume.Core.Utils.OneList


class StateMap f where
  stmap :: (a -> st -> (b,st)) -> f a -> st -> (f b,st)


-- Bifunctor...
class StateMap2 f where
  stmap2 :: (a -> st -> (u,st)) -> (b -> st -> (v,st)) -> f a b -> st -> (f u v, st)
 
class StateMap3 f where
  stmap3 :: (a -> st -> (u,st)) -> (b -> st -> (v,st)) -> (c -> st -> (w,st)) 
         -> f a b c -> st -> (f u v w, st)

stmap2a :: StateMap2 f => (a -> st -> (u,st)) -> f a b -> st -> (f u b, st)
stmap2a f = stmap2 f (,)

stmap2b :: StateMap2 f => (b -> st -> (v,st)) -> f a b -> st -> (f a v, st)
stmap2b g = stmap2 (,) g


stmap3a :: StateMap3 f => (a -> st -> (u,st)) -> f a b c -> st -> (f u b c, st)
stmap3a f1 = stmap3 f1 (,) (,)

stmap3b :: StateMap3 f => (b -> st -> (v,st)) -> f a b c -> st -> (f a v c, st)
stmap3b f2 = stmap3 (,) f2 (,)

stmap3c :: StateMap3 f => (c -> st -> (w,st)) -> f a b c -> st -> (f a b w, st)
stmap3c f3 = stmap3 (,) (,) f3

--------------------------------------------------------------------------------
-- Instances

instance StateMap [] where
  stmap _ []     st = ([],st)
  stmap f (x:xs) st = (b:bs,st'') where (b,st')   = f x st  
                                        (bs,st'') = stmap f xs st'

instance StateMap Maybe where
  stmap _ Nothing  st = (Nothing,st)
  stmap f (Just a) st = (Just a',st') where (a',st') = f a st

instance StateMap ((,) a) where
  stmap f (x,y) st   = ((x,y'),st') where (y',st') = f y st

instance StateMap OneList where
  stmap f os s0 = step (viewl os) s0 where
    step (OneL a)   st = (one a', st')     where (a',st')  = f a st
    step (a :<< as) st = (cons b bs,st'')  where (b,st')   = f a st  
                                                 (bs,st'') = step (viewl as) st'


-- StateMap2

instance StateMap2 (,) where 
  stmap2 f g (x,y) st   = ((x',y'),st'') where (x',st')  = f x st
                                               (y',st'') = g y st'

instance StateMap2 Either where
  stmap2 f _ (Left x)  st   = (Left x',st')  where (x',st') = f x st
  stmap2 _ g (Right y) st   = (Right y',st') where (y',st') = g y st


