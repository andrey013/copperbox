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
    stcomb
  , stcombWith
  , psimap_st
  , caboose_stmap
  , anacrusis_stmap

  -- * Stateful map
  , StateMap(..)
  , StateMap2(..)
  , StateMap3(..)

  , stmap2a
  , stmap2b

  , stmap3a 
  , stmap3b
  , stmap3c

  ) where 

import Neume.Core.Utils.OneList



stcomb :: (st -> (a,st)) -> (st -> (b,st)) -> st -> ((a,b),st)
stcomb f g st = ((a,b),st'') where
    (a,st')  = f st
    (b,st'') = g st'

stcombWith :: (a -> b -> c) -> (st -> (a,st)) -> (st -> (b,st)) -> st -> (c,st)
stcombWith op f g st = (a `op` b,st'') where
    (a,st')  = f st
    (b,st'') = g st'


-- Rather like on (aka psi) from Data.Function 
psimap_st :: (st -> a -> (b,st)) -> st -> a -> [a] -> ((b,[b]),st)
psimap_st f st x xs = ((b,bs),st'') where
   (b,st')   = f st x 
   (bs,st'') = stmap f st' xs

-- just on lists...
caboose_stmap :: (st -> a -> (b,st)) -> (st -> a -> (b,st)) -> st -> [a] -> ([b],st)
caboose_stmap _ _ s0 []     = ([],s0)
caboose_stmap f g s0 (a:as) = step s0 a as where
  step st x []           = ([z],st') where (z,st') = g st x
  step st x (y:ys)       = (z:zs,st'') 
                            where (z,st')   = f st x 
                                  (zs,st'') = step st' y ys

anacrusis_stmap :: (st -> a -> (b,st)) -> (st -> a -> (b,st)) -> st -> [a] -> ([b],st)
anacrusis_stmap _ _ s0 []     = ([],s0)
anacrusis_stmap f g s0 (a:as) = (x:xs,st'') 
  where
    (x,st')   = f s0 a
    (xs,st'') = stmap g st' as




--------------------------------------------------------------------------------

class StateMap f where
  stmap :: (st -> a -> (b,st)) -> st -> f a -> (f b,st)


-- Bifunctor...
class StateMap2 f where
  stmap2 :: (st -> a -> (u,st)) -> (st -> b -> (v,st)) -> st -> f a b -> (f u v, st)
 
class StateMap3 f where
  stmap3 :: (st -> a -> (u,st)) -> (st -> b -> (v,st)) -> (st -> c -> (w,st)) 
         -> st -> f a b c -> (f u v w, st)


stmap2a :: StateMap2 f => (st -> a -> (u,st)) -> st -> f a b -> (f u b, st)
stmap2a f = stmap2 f revpair

stmap2b :: StateMap2 f => (st -> b -> (v,st)) -> st -> f a b -> (f a v, st)
stmap2b g = stmap2 revpair g


stmap3a :: StateMap3 f => (st -> a -> (u,st)) -> st -> f a b c -> (f u b c, st)
stmap3a f1 = stmap3 f1 revpair revpair

stmap3b :: StateMap3 f => (st -> b -> (v,st)) -> st -> f a b c -> (f a v c, st)
stmap3b f2 = stmap3 revpair f2 revpair

stmap3c :: StateMap3 f => (st -> c -> (w,st)) -> st -> f a b c -> (f a b w, st)
stmap3c f3 = stmap3 revpair revpair f3

revpair :: a -> b -> (b,a) 
revpair = flip (,)

--------------------------------------------------------------------------------
-- Instances

instance StateMap [] where
  stmap _ st []     = ([],st)
  stmap f st (x:xs) = (b:bs,st'') where (b,st')   = f st x
                                        (bs,st'') = stmap f st' xs

instance StateMap Maybe where
  stmap _ st Nothing  = (Nothing,st)
  stmap f st (Just a) = (Just a',st') where (a',st') = f st a


instance StateMap OneList where
  stmap f s0 os = step s0 (viewl os) where
    step st (OneL a)   = (one a', st')     where (a',st')  = f st a
    step st (a :<< as) = (cons b bs,st'')  where (b,st')   = f st a 
                                                 (bs,st'') = step st' (viewl as)

-- StateMap2

instance StateMap2 (,) where 
  stmap2 f g st (x,y)   = ((x',y'),st'') where (x',st')  = f st x 
                                               (y',st'') = g st' y

instance StateMap2 Either where
  stmap2 f _ st (Left x)    = (Left x',st')  where (x',st') = f st x
  stmap2 _ g st (Right y)   = (Right y',st') where (y',st') = g st y


